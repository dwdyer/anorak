{-# LANGUAGE DeriveDataTypeable #-}

-- | A module for parsing RLT data files as used by the Football Statistics Applet (see <https://fsa.dev.java.net>).
module Anorak.RLTParser (LeagueData(LeagueData), parseRLTFile, RLTException) where

import Anorak.Results
import Control.Exception(Exception, throw)
import Data.Map(Map)
import qualified Data.Map as Map(empty, insertWith, unionWith)
import Data.Set(Set)
import qualified Data.Set as Set(empty, fromList, insert, union)
import Data.Time.Format(readTime)
import Data.Typeable(Typeable)
import List(concat, intersperse)
import System.FilePath(combine, takeDirectory)
import System.Locale(defaultTimeLocale)
import System.IO.Unsafe(unsafePerformIO)
import Text.ParserCombinators.Parsec((<|>), anyChar, char, eof, many1, manyTill, newline, noneOf, ParseError, parseFromFile, Parser, sepBy1)

-- | An RLT file consists of many items (results, metadata and comments).
data Item = Fixture Result               -- ^ The result of a single football match.
          | Include FilePath             -- ^ The path to another RLT file that should be parsed and included.
          | Adjustment Team Int          -- ^ A number of points awarded to or deducted from an individual team.
          | MiniLeague String (Set Team) -- ^ A league within a league (e.g. The Big 4, London Clubs, North West Clubs, etc.)
          | Rules Int Int Int            -- ^ Number of points for a win, points for a draw, and split point (zero for non-SPL-style leagues)
          | Metadata [String]            -- ^ Data about the league, such as which positions are promoted or relegated.
          | Comment String               -- ^ Comments about the data.

-- | An RLTException is thrown when there is a problem parsing RLT input.
data RLTException = RLTException ParseError deriving (Typeable, Show)
instance Exception RLTException 

-- | League data is extracted from an RLT file.  It consists of a list of teams, a list of results,
--   a (probably empty) map of points adjustments, a list of mini-leagues, and the SPL-style split point (zero for sane leagues).
data LeagueData = LeagueData (Set Team) [Result] (Map Team Int) [(String, Set Team)] Int

-- | Parse results and points adjustments, discard meta-data and comments.  The function argument is
--   the path of the data directory, used to resolve relative paths for include directives.
results :: FilePath -> Parser (LeagueData)
results dataDir = do list <- items
                     return (extractData dataDir list)

-- | Each line of a data file is either a record (a match result or some metadata) or it is a comment.
items :: Parser [Item]
items = manyTill (comment <|> record) eof

-- | A record is a list of fields delimited by pipe characters.
record :: Parser Item
record = do fields <- sepBy1 field (char '|')
            newline
            case fields of
                ("INCLUDE":path:[])                -> return $ Include path
                ("AWARDED":team:points:[])         -> return $ Adjustment team $ read points
                ("DEDUCTED":team:points:[])        -> return $ Adjustment team $ -read points
                ("MINILEAGUE":name:teams)          -> return $ MiniLeague name $ Set.fromList teams
                ("PRIZE":_)                        -> return $ Metadata fields
                ("RELEGATION":_)                   -> return $ Metadata fields
                ("RULES":win:draw:split:[])        -> return $ Rules (read win) (read draw) (read split)
                (date:hTeam:hGoals:aTeam:aGoals:_) -> return $ Fixture $ Result day hTeam (read hGoals) aTeam (read aGoals)
                                                      -- RLT dates are 8-character strings in DDMMYYYY format.
                                                      where day = readTime defaultTimeLocale "%d%m%Y" date
                _                                  -> fail $ "Unexpected input: " ++ concat (intersperse "|" fields)

-- | A field is one or more characters (not including pipes and newlines).
field :: Parser String
field = many1 (noneOf "|\n")

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
                                                           (LeagueData t' r' a' _ s') = unsafePerformIO $ parseRLTFile $ combine dataDir path
extractData dataDir (Adjustment team amount:items) = LeagueData t r (Map.insertWith (+) team amount a) m s
                                                     where (LeagueData t r a m s) = extractData dataDir items
extractData dataDir (MiniLeague name teams:items)  = LeagueData t r a ((name, teams):m) s
                                                     where (LeagueData t r a m s) = extractData dataDir items
extractData dataDir (Rules _ _ split:items)        = LeagueData t r a m split
                                                     where (LeagueData t r a m _) = extractData dataDir items
extractData dataDir (_:items)                      = extractData dataDir items -- Discard metadata.

-- | Adds the home team and away team from a match to the set of all teams (if they are not already present).
addTeams :: Result -> Set Team -> Set Team
addTeams result = Set.insert (awayTeam result) . Set.insert (homeTeam result)

-- | Parses the specified RLT file and returns a list of teams, a list of results, a map of any points adjustments,
--   and a list of any configured mini-leagues (each represented by name/teams pair).
--   Throws an RLTException if there is a problem parsing the file.
parseRLTFile :: FilePath -> IO (LeagueData)
parseRLTFile path = do let dataDir = takeDirectory path
                       contents <- parseFromFile (results dataDir) path
                       case contents of
                           Left error        -> throw $ RLTException error
                           Right leagueData  -> return leagueData

