{-# LANGUAGE DeriveDataTypeable #-}

-- | A module for parsing RLT data files (as used by the Football Statistics Applet <https://fsa.dev.java.net>).
module Anorak.RLTParser (parseRLTFile, RLTException) where

import Anorak.Types
import Control.Exception
import Data.Map(Map)
import qualified Data.Map as Map
import Data.Set(Set)
import qualified Data.Set as Set
import Data.Time.Calendar(Day)
import Data.Time.Format(readTime)
import Data.Typeable(Typeable)
import List(concat, intersperse, sort)
import System.Locale(defaultTimeLocale)
import Text.ParserCombinators.Parsec

-- | An RLT file consists of many items (results, metadata and comments).
data Item = Fixture Result        -- ^ The result of a single football match.
          | Adjustment Team Int   -- ^ A number of points awarded to or deducted from an individual team.
          | Metadata [String]     -- ^ Data about the league, such as which positions are promoted or relegated.
          | Comment String        -- ^ Comments about the data.

-- | An RLTException is thrown when there is a problem parsing RLT input.
data RLTException = RLTException ParseError deriving (Typeable, Show)
instance Exception RLTException 

-- | Parse results and points adjustments, discard meta-data and comments.
results :: Parser ([Team], [Result], Map Team Int)
results = do list <- items
             let (teams, results, adjustments) = extractData list (Set.empty, [], Map.empty) in
                 return (Set.elems teams, results, adjustments)

-- | Each line of a data file is either a record (a match result or some metadata) or it is a comment.
items :: Parser [Item]
items = manyTill (comment <|> record) eof

-- | A record is a list of fields delimited by pipe characters.
record :: Parser Item
record = do fields <- sepBy1 field (char '|')
            newline
            case fields of
                (date:hTeam:hGoals:aTeam:aGoals:_) -> return (Fixture (Result day hTeam (read hGoals) aTeam (read aGoals)))
                                                      -- RLT dates are 8-character strings in DDMMYYYY format.
                                                      where day = readTime defaultTimeLocale "%d%m%Y" date
                ("AWARDED":team:points:[])         -> return (Adjustment team (read points))
                ("DEDUCTED":team:points:[])        -> return (Adjustment team (-read points))
                ("PRIZE":_)                        -> return (Metadata fields)
                ("RELEGATION":_)                   -> return (Metadata fields)
                ("RULES":_)                        -> return (Metadata fields)
                otherwise                          -> fail ("Unexpected input: " ++ (concat $ intersperse "|" fields))

field = many1 (noneOf "|\n")

-- | A comment starts with a hash and continues to the end of the line.
comment :: Parser Item
comment = do char '#'
             text <- manyTill anyChar newline
             return (Comment text)

-- | Takes a list of parsed items and discards comments and meta-data.  The remaining items are separated into a list
--   of results and a map of net points adjustments by team.
extractData :: [Item] -> (Set Team, [Result], Map Team Int) -> (Set Team, [Result], Map Team Int)
extractData [] (t, r, a)                             = (t, sort r, a) -- Sort results as the final operation because otherwise they are backwards.
extractData (Fixture result:items) (t, r, a)         = extractData items (addTeams result t, result:r, a)
extractData (Adjustment team amount:items) (t, r, a) = extractData items (t, r, addAdjustment team amount a)
extractData (_:items) (t, r, a)                      = extractData items (t, r, a)

-- | Adds the home team and away team from a match to the set of all teams (if they are not already present).
addTeams :: Result -> Set Team -> Set Team
addTeams result set = Set.insert (awayTeam result) (Set.insert (homeTeam result) set)

-- | Adds a points adjustment to a map of existing points adjustments.  All adjustments for an individual team are
--   collated into a single entry.
addAdjustment :: Team -> Int -> Map Team Int -> Map Team Int
addAdjustment team amount map = Map.insert team total map
                                where total = amount + Map.findWithDefault 0 team map 

-- | Parses the given string (the content of an RLT file) and returns a list of football results and a map of any points
--   adjustments included in the data.
parseRLT :: String -> ([Team], [Result], Map Team Int)
parseRLT input = case parse results "(unknown)" input of
                     Left error      -> throw (RLTException error)
                     Right (t, r, a) -> (t, r, a)

-- | Parses the specified RLT file and returns a list of the results it contains and a map of any points adjustments.
parseRLTFile :: FilePath -> IO ([Team], [Result], Map Team Int)
parseRLTFile path = do contents <- parseFromFile results path
                       case contents of
                           Left error      -> throw (RLTException error)
                           Right (t, r, a) -> return (t, r, a)

