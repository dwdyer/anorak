{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}

-- | A module for parsing RLT data files as used by the Football Statistics Applet (see <https://fsa.dev.java.net>).
module Anorak.RLTParser (LeagueData(..),
                         parseRLTFile,
                         RLTException) where

import Anorak.Results
import Control.Applicative((<|>), (<*))
import Control.Exception(Exception, throw)
import Data.Attoparsec.Char8(char, digit, endOfLine, notInClass, parse, Parser, decimal, string, takeTill, takeWhile1, IResult(..))
import Data.Attoparsec.Combinator(count, option, sepBy, sepBy1)
import Data.ByteString.Char8(ByteString)
import qualified Data.ByteString.Char8 as BS(empty, readFile, unpack)
import Data.Map(Map)
import qualified Data.Map as Map(empty, insert, insertWith, unionWith)
import Data.Set(Set)
import qualified Data.Set as Set(empty, fromList, insert, union)
import Data.Time.Calendar(fromGregorian)
import Data.Typeable(Typeable)
import System.FilePath(combine, takeDirectory)
import System.IO.Unsafe(unsafePerformIO)

-- | An RLT file consists of many items (results, metadata and comments).
data Item = Fixture Result                    -- ^ The result of a single football match.
          | Include FilePath                  -- ^ The path to another RLT file that should be parsed and included.
          | Adjustment Team Int               -- ^ A number of points awarded to or deducted from an individual team.
          | MiniLeague ByteString (Set Team)  -- ^ A league within a league (e.g. The Big 4, London Clubs, North West Clubs, etc.)
          | Rules Int Int Int                 -- ^ Number of points for a win, points for a draw, and split point (zero for non-SPL-style leagues)
          | Alias ByteString Team             -- ^ Maps a team name to an alias (used when teams change names, i.e. Wimbledon to MK Dons)
          | PrizeZone Int Int ByteString      -- ^ Promotion/play-off/cup qualification/other prize zone in a league table.
          | RelegationZone Int Int ByteString -- ^ Relegation/relegation play-off zone in a league table.
          | Comment ByteString                -- ^ Comments about the data.
    deriving (Show)

-- | An RLTException is thrown when there is a problem parsing RLT input.
data RLTException = RLTException String deriving (Typeable, Show)
instance Exception RLTException 

-- | League data is extracted from an RLT file.  It consists of a list of teams, a list of results,
--   a (probably empty) map of points adjustments, a list of mini-leagues, and the SPL-style split point (zero for sane leagues).
data LeagueData = LeagueData {teams :: Set Team,
                              results :: [Result],
                              adjustments :: Map Team Int,
                              miniLeagues :: [(ByteString, Set Team)],
                              split :: Int,
                              aliases :: Map ByteString Team,
                              zones :: Map String String
                             }

-- | Parse results, points adjustments, and meta-data, discard comments.  The function argument is
--   the path of the data directory, used to resolve relative paths for include directives.
leagueData :: FilePath -> Parser LeagueData
leagueData dataDir = do itemList <- items
                        return (extractData dataDir itemList)

-- | Each line of a data file is either a record (a match result or some metadata) or it is a comment.
items :: Parser [Item]
items = sepBy (result <|> comment <|> include <|> rules <|> awarded <|> deducted <|> miniLeague <|> prize <|> relegation <|> alias) endOfLine

-- | The INCLUDE directive has a single field, the path to an RLT file.
include :: Parser Item
include = do consume $ string "INCLUDE|"
             path <- takeTill isNewLine
             return . Include $ BS.unpack path

-- | The AWARDED directive adds points to a team's total.  It has two fields, team name and number of points.
awarded :: Parser Item
awarded = do consume $ string "AWARDED|"
             team <- pipeTerminated
             amount <- decimal;
             return $ Adjustment team amount

-- | The DEDUCTED directive removes points to a teams' total.  It has two fields, team name and number of points.
deducted :: Parser Item
deducted = do consume $ string "DEDUCTED|"
              team <- pipeTerminated
              amount <- decimal;
              return $ Adjustment team (-amount)

-- | The first field of the MINILEAGUE directive is the league's name, other fields are the member teams.
miniLeague :: Parser Item
miniLeague = do consume $ string "MINILEAGUE|"
                name <- pipeTerminated
                members <- sepBy1 (takeWhile1 (notInClass "|\n")) pipe;
                return . MiniLeague name $ Set.fromList members

-- | The optional RULES directive has three numeric fields, number of points for a win, number of points for a draw
--   and number of games played by each team before the league splits in half (only applicable for the Scottish
--   Premier League and similarly structured leagues, all others should be set to zero).
rules :: Parser Item
rules = do consume $ string "RULES|"
           win <- decimal; pipe
           draw <- decimal; pipe
           splitPoint <- decimal;
           return $ Rules win draw splitPoint

-- | The PRIZE directive identifies zones at the top of a division, first field is start position, second is end, third is name.
prize :: Parser Item
prize = do consume $ string "PRIZE|"
           start <- decimal; pipe
           end <- decimal; pipe
           name <- takeTill isNewLine;
           return $ PrizeZone start end name

-- | The RELEGATION directive identifies zones at the top of a division, first field is start position, second is end, third is name.
relegation :: Parser Item
relegation = do consume $ string "RELEGATION|"
                start <- decimal; pipe
                end <- decimal; pipe
                name <- takeTill isNewLine; 
                return $ RelegationZone start end name

-- | The ALIAS directive maps a team name to a team's previous name.
alias :: Parser Item
alias = do consume $ string "ALIAS|"
           name <- pipeTerminated
           team <- takeTill isNewLine;
           return $ Alias name team

-- | A record is a list of fields delimited by pipe characters.
result :: Parser Item
result = do dayOfMonth <- count 2 digit
            month <- count 2 digit
            year <- count 4 digit; pipe
            hTeam <- pipeTerminated
            hGoals <- score; pipe
            aTeam <- pipeTerminated
            aGoals <- score;
            let matchDate = fromGregorian (read year) (read month) (read dayOfMonth)
            return . Fixture $ Result matchDate hTeam (fst hGoals) aTeam (fst aGoals) (snd hGoals) (snd aGoals)

-- | A score is the number of goals scored by one team in a game and, depending on the detail in the data, a list
--   of the scorers and goal times.
score :: Parser (Int, [Goal])
score = do number <- decimal
           goalsList <- option [] parseGoals
           return (number, goalsList)
           where parseGoals = do {consume $ char '['; gs <- sepBy1 goal $ char ','; consume $ char ']'; return gs}

goal :: Parser Goal
goal = do player <- takeWhile1 (notInClass "0123456789,")
          gMinute <- decimal
          gType <- string "p" <|> string "o" <|> string ""
          return $ Goal player gMinute gType

-- | A comment starts with a hash and continues to the end of the line.
comment :: Parser Item
comment = do consume $ char '#'
             text <- takeTill isNewLine;
             return $ Comment text

-- | Parses a pipe-terminated field, returning its contents and consuming the pipe character.
pipeTerminated :: Parser ByteString
pipeTerminated = takeTill isPipe <* pipe
                 where isPipe c = c == '|'

pipe :: Parser ()
pipe = consume $ char '|'

isNewLine :: Char -> Bool
isNewLine c = c `elem` "\n\r"

-- | Discard the input matched by the specified parser.
consume :: Parser a -> Parser ()
consume p = do _ <- p
               return ()

-- | Takes a list of parsed items and discards comments and meta-data.  The remaining items are separated into a list
--   of results and a map of net points adjustments by team.
extractData :: FilePath -> [Item] -> LeagueData
extractData _ []                                      = LeagueData Set.empty [] Map.empty [] 0 Map.empty Map.empty
extractData dataDir (Fixture matchResult:records)     = LeagueData (addTeams matchResult t) (matchResult:r) a m s al z
                                                        where (LeagueData t r a m s al z) = extractData dataDir records
extractData dataDir (Include path:records)            = LeagueData (t `Set.union` t') (r' ++ r) (Map.unionWith (+) a a') m s al z
                                                        where (LeagueData t r a m s al z) = extractData dataDir records
                                                              (LeagueData t' r' a' _ _ _ _) = unsafePerformIO . parseRLTFile $ combine dataDir path
extractData dataDir (Adjustment team amount:records)  = LeagueData t r (Map.insertWith (+) team amount a) m s al z
                                                        where (LeagueData t r a m s al z) = extractData dataDir records
extractData dataDir (MiniLeague name members:records) = LeagueData t r a ((name, members):m) s al z
                                                        where (LeagueData t r a m s al z) = extractData dataDir records
extractData dataDir (Rules _ _ sp:records)            = LeagueData t r a m sp al z
                                                        where (LeagueData t r a m _ al z) = extractData dataDir records
extractData dataDir (Alias name team:records)         = LeagueData t r a m s (Map.insert name team al) z
                                                        where (LeagueData t r a m s al z) = extractData dataDir records
extractData dataDir (_:records)                       = extractData dataDir records -- Discard comments.

-- | Adds the home team and away team from a match to the set of all teams (if they are not already present).
addTeams :: Result -> Set Team -> Set Team
addTeams matchResult = Set.insert (awayTeam matchResult) . Set.insert (homeTeam matchResult)

-- | Parses the specified RLT file and returns a list of teams, a list of results, a map of any points adjustments,
--   and a list of any configured mini-leagues (each represented by name/teams pair).
--   Throws an RLTException if there is a problem parsing the file.
parseRLTFile :: FilePath -> IO LeagueData
parseRLTFile path = do let dataDir = takeDirectory path
                       contents <- BS.readFile path
                       let parseResult = parse (leagueData dataDir) contents
                       case parseResult of
                            Fail _ _ err  -> throw $ RLTException err
                            Partial p     -> case p BS.empty of
                                                  Fail _ _ err  -> throw $ RLTException err
                                                  Partial _     -> throw $ RLTException "Incomplete input."
                                                  Done _ lgData -> return lgData
                            Done _ lgData -> return lgData

