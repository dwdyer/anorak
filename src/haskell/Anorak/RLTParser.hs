{-# LANGUAGE DeriveDataTypeable #-}

-- | A module for parsing RLT data files as used by the Football Statistics Applet (see <https://fsa.dev.java.net>).
module Anorak.RLTParser (LeagueData(LeagueData), parseRLTFile, RLTException) where

import Anorak.Results
import Control.Exception(Exception, throw)
import Data.Map(Map)
import qualified Data.Map as Map(empty, insertWith)
import Data.Set(Set)
import qualified Data.Set as Set(elems, empty, fromList, insert)
import Data.Time.Calendar(Day)
import Data.Time.Format(readTime)
import Data.Typeable(Typeable)
import List(concat, intersperse)
import System.Locale(defaultTimeLocale)
import Text.ParserCombinators.Parsec((<|>), anyChar, char, eof, many1, manyTill, newline, noneOf, ParseError, parseFromFile, Parser, sepBy1)

-- | An RLT file consists of many items (results, metadata and comments).
data Item = Fixture Result               -- ^ The result of a single football match.
          | Adjustment Team Int          -- ^ A number of points awarded to or deducted from an individual team.
          | MiniLeague String (Set Team) -- ^ A league within a league (e.g. The Big 4, London Clubs, North West Clubs, etc.)
          | Metadata [String]            -- ^ Data about the league, such as which positions are promoted or relegated.
          | Comment String               -- ^ Comments about the data.

-- | An RLTException is thrown when there is a problem parsing RLT input.
data RLTException = RLTException ParseError deriving (Typeable, Show)
instance Exception RLTException 

-- | League data is extracted from an RLT file.  It consists of a list of teams, a list of results,
--   a (probably empty) map of points adjustments, and a list of mini-leagues.
data LeagueData = LeagueData (Set Team) [Result] (Map Team Int) [(String, Set Team)]

-- | Parse results and points adjustments, discard meta-data and comments.
results :: Parser (LeagueData)
results = do list <- items
             return (extractData list)

-- | Each line of a data file is either a record (a match result or some metadata) or it is a comment.
items :: Parser [Item]
items = manyTill (comment <|> record) eof

-- | A record is a list of fields delimited by pipe characters.
record :: Parser Item
record = do fields <- sepBy1 field (char '|')
            newline
            case fields of
                ("AWARDED":team:points:[])         -> return $ Adjustment team $ read points
                ("DEDUCTED":team:points:[])        -> return $ Adjustment team $ -read points
                ("MINILEAGUE":name:teams)          -> return $ MiniLeague name $ Set.fromList teams
                ("PRIZE":_)                        -> return $ Metadata fields
                ("RELEGATION":_)                   -> return $ Metadata fields
                ("RULES":_)                        -> return $ Metadata fields
                (date:hTeam:hGoals:aTeam:aGoals:_) -> return $ Fixture $ Result day hTeam (read hGoals) aTeam (read aGoals)
                                                      -- RLT dates are 8-character strings in DDMMYYYY format.
                                                      where day = readTime defaultTimeLocale "%d%m%Y" date
                otherwise                          -> fail $ "Unexpected input: " ++ (concat $ intersperse "|" fields)

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
extractData :: [Item] -> LeagueData
extractData []                             = (LeagueData Set.empty [] Map.empty []) 
extractData (Fixture result:items)         = (LeagueData (addTeams result t) (result:r) a m) where (LeagueData t r a m) = extractData items
extractData (Adjustment team amount:items) = (LeagueData t r (Map.insertWith (+) team amount a) m) where (LeagueData t r a m) = extractData items
extractData (MiniLeague name teams:items)  = (LeagueData t r a ((name, teams):m)) where (LeagueData t r a m) = extractData items
extractData (_:items)                      = extractData items -- Discard metadata.

-- | Adds the home team and away team from a match to the set of all teams (if they are not already present).
addTeams :: Result -> Set Team -> Set Team
addTeams result set = Set.insert (awayTeam result) (Set.insert (homeTeam result) set)

-- | Parses the specified RLT file and returns a list of teams, a list of results, a map of any points adjustments,
--   and a list of any configured mini-leagues (each represented by name/teams pair).
--   Throws an RLTException if there is a problem parsing the file.
parseRLTFile :: FilePath -> IO (LeagueData)
parseRLTFile path = do contents <- parseFromFile results path
                       case contents of
                           Left error        -> throw $ RLTException error
                           Right leagueData  -> return leagueData

