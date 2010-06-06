{-# LANGUAGE DeriveDataTypeable #-}

-- | A module for parsing RLT data files as used by the Football Statistics Applet (see <https://fsa.dev.java.net>).
module Anorak.RLTParser (LeagueData(LeagueData), parseRLTFile, RLTException) where

import Anorak.Results
import Control.Exception(Exception, throw)
import Data.List(concat, foldl', intersperse)
import Data.Map(Map)
import qualified Data.Map as Map(empty, insertWith, unionWith)
import Data.Set(Set)
import qualified Data.Set as Set(empty, fromList, insert, union)
import Data.Time.Calendar(Day, fromGregorian)
import Data.Typeable(Typeable)
import System.FilePath(combine, takeDirectory)
import System.IO.Unsafe(unsafePerformIO)
import Text.ParserCombinators.Parsec((<|>), anyChar, between, char, count, digit, eof, many1, manyTill, newline, noneOf, option, ParseError, parseFromFile, Parser, sepBy, sepBy1, spaces, string, try)

-- | An RLT file consists of many items (results, metadata and comments).
data Item = Fixture Result               -- ^ The result of a single football match.
          | Include FilePath             -- ^ The path to another RLT file that should be parsed and included.
          | Adjustment Team Int          -- ^ A number of points awarded to or deducted from an individual team.
          | MiniLeague String (Set Team) -- ^ A league within a league (e.g. The Big 4, London Clubs, North West Clubs, etc.)
          | Rules Int Int Int            -- ^ Number of points for a win, points for a draw, and split point (zero for non-SPL-style leagues)
          | Comment String               -- ^ Comments about the data.

-- | An RLTException is thrown when there is a problem parsing RLT input.
data RLTException = RLTException ParseError deriving (Typeable, Show)
instance Exception RLTException 

-- | League data is extracted from an RLT file.  It consists of a list of teams, a list of results,
--   a (probably empty) map of points adjustments, a list of mini-leagues, and the SPL-style split point (zero for sane leagues).
data LeagueData = LeagueData (Set Team) [Result] (Map Team Int) [(String, Set Team)] Int

-- | Parse results and points adjustments, discard meta-data and comments.  The function argument is
--   the path of the data directory, used to resolve relative paths for include directives.
results :: FilePath -> Parser LeagueData
results dataDir = do list <- items
                     return (extractData dataDir list)

-- | Each line of a data file is either a record (a match result or some metadata) or it is a comment.
items :: Parser [Item]
items = manyTill (result <|> comment <|> include <|> try rules <|> awarded <|> deducted <|> miniLeague <|> prize <|> relegation <|> fail "Unexpected record type.") eof

-- | The INCLUDE directive has a single field, the path to an RLT file.
include :: Parser Item
include = do string "INCLUDE|"
             path <- manyTill anyChar newline
             return $ Include path

-- | The AWARDED directive adds points to a teams' total.  It has two fields, team name and number of points.
awarded :: Parser Item
awarded = do string "AWARDED|"
             team <- manyTill anyChar $ char '|'
             amount <- manyTill digit newline
             return . Adjustment team $ read amount

-- | The DEDUCTED directive removes points to a teams' total.  It has two fields, team name and number of points.
deducted :: Parser Item
deducted = do string "DEDUCTED|"
              team <- manyTill anyChar $ char '|'
              amount <- manyTill digit newline
              return . Adjustment team $ -read amount

-- | The first field of the MINILEAGUE directive is the league's name, other fields are the member teams.
miniLeague :: Parser Item
miniLeague = do string "MINILEAGUE|"
                name <- manyTill anyChar $ char '|'
                teams <- sepBy1 (many1 $ noneOf "|\n") $ char '|' ; newline
                return . MiniLeague name $ Set.fromList teams

-- | The optional RULES directive has three numeric fields, number of points for a win, number of points for a draw
--   and number of games played by each team before the league splits in half (only applicable for the Scottish
--   Premier League and similarly structured leagues, all others should be set to zero).
rules :: Parser Item
rules = do string "RULES|"
           win <- manyTill digit $ char '|'
           draw <- manyTill digit $ char '|'
           split <- manyTill digit newline
           return $ Rules (read win) (read draw) (read split)

-- | The PRIZE directive identifies zones at the top of a division, first field is start position, second is end, third is name.
prize :: Parser Item
prize = do string "PRIZE|"
           start <- manyTill digit $ char '|'
           end <- manyTill digit $ char '|'
           name <- manyTill anyChar newline
           return $ Comment name -- Treat as a comment, it's ignored for now.

-- | The RELEGATION directive identifies zones at the top of a division, first field is start position, second is end, third is name.
relegation :: Parser Item
relegation = do string "RELEGATION|"
                start <- manyTill digit $ char '|'
                end <- manyTill digit $ char '|'
                name <- manyTill anyChar newline
                return $ Comment name -- Treat as a comment, it's ignored for now.

-- | A record is a list of fields delimited by pipe characters.
result :: Parser Item
result = do date <- rltDate ; char '|'
            hTeam <- manyTill anyChar $ char '|'
            hGoals <- score ; char '|'
            aTeam <- manyTill anyChar $ char '|'
            aGoals <- score ; newline
            return . Fixture $ Result date hTeam (fst hGoals) aTeam (fst aGoals) (snd hGoals) (snd aGoals)

-- | A score is the number of goals scored by one team in a game and, depending on the detail in the data, a list
--   of the scorers and goal times.
score :: Parser (Int, [Goal])
score = do number <- many1 digit
           goals <- option [] $ between (char '[') (char ']') goals
           return (read number, goals)
           where goals = sepBy goal $ char ','

goal :: Parser Goal
goal = do player <- many1 $ noneOf "0123456789,"
          minute <- many1 digit;
          goalType <- string "p" <|> string "o" <|> string ""
          return $ Goal player (read minute) goalType

-- | RLT dates are 8-character strings in DDMMYYYY format.
rltDate :: Parser Day
rltDate = do day <- count 2 digit
             month <- count 2 digit
             year <- count 4 digit
             return $ fromGregorian (read year) (read month) (read day)

-- | A comment starts with a hash and continues to the end of the line.
comment :: Parser Item
comment = do char '#'
             text <- manyTill anyChar newline
             return (Comment text)

-- | Takes a list of parsed items and discards comments and meta-data.  The remaining items are separated into a list
--   of results and a map of net points adjustments by team.
extractData :: FilePath -> [Item] -> LeagueData
extractData _ []                                   = LeagueData Set.empty [] Map.empty [] 0
extractData dataDir (Fixture result:items)         = LeagueData (addTeams result t) (result:r) a m s
                                                     where (LeagueData t r a m s) = extractData dataDir items
extractData dataDir (Include path:items)           = LeagueData (Set.union t t') (r' ++ r) (Map.unionWith (+) a a') m s
                                                     where (LeagueData t r a m s) = extractData dataDir items
                                                           (LeagueData t' r' a' _ s') = unsafePerformIO . parseRLTFile $ combine dataDir path
extractData dataDir (Adjustment team amount:items) = LeagueData t r (Map.insertWith (+) team amount a) m s
                                                     where (LeagueData t r a m s) = extractData dataDir items
extractData dataDir (MiniLeague name teams:items)  = LeagueData t r a ((name, teams):m) s
                                                     where (LeagueData t r a m s) = extractData dataDir items
extractData dataDir (Rules _ _ split:items)        = LeagueData t r a m split
                                                     where (LeagueData t r a m _) = extractData dataDir items
extractData dataDir (_:items)                      = extractData dataDir items -- Discard comments.

-- | Adds the home team and away team from a match to the set of all teams (if they are not already present).
addTeams :: Result -> Set Team -> Set Team
addTeams result = Set.insert (awayTeam result) . Set.insert (homeTeam result)

-- | Parses the specified RLT file and returns a list of teams, a list of results, a map of any points adjustments,
--   and a list of any configured mini-leagues (each represented by name/teams pair).
--   Throws an RLTException if there is a problem parsing the file.
parseRLTFile :: FilePath -> IO LeagueData
parseRLTFile path = do let dataDir = takeDirectory path
                       contents <- parseFromFile (results dataDir) path
                       case contents of
                           Left error       -> throw $ RLTException error
                           Right leagueData -> return leagueData

