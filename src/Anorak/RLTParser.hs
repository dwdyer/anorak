-- | A module for parsing RLT data files (as used by the Football Statistics Applet <https://fsa.dev.java.net>).
module Anorak.RLTParser (parseRLT) where

import Anorak.Types
import Data.Map(Map)
import qualified Data.Map as Map
import Data.Time.Calendar(Day)
import Data.Time.Format(readTime)
import List(sort)
import System.Locale(defaultTimeLocale)
import Text.ParserCombinators.Parsec

-- | An RLT file consists of many items (results, metadata and comments).
data Item = Fixture Result        -- ^ The result of a single football match.
          | Adjustment Team Int   -- ^ A number of points awarded to or deducted from an individual team.
          | Metadata [String]     -- ^ Data about the league, such as which positions are promoted or relegated.
          | Comment String        -- ^ Comments about the data.

-- | Parse results, discard meta-data and comments.
results :: Parser ([Result], Map Team Int)
results = do list <- items
             return (extractData list ([], Map.empty))

-- | Each line of a data file is either a record (a match result or some metadata) or it is a comment.
items :: Parser [Item]
items = many (comment <|> record)

-- | A record is a list of fields delimited by pipe characters.
record :: Parser Item
record = do fields <- sepBy field (char '|')
            newline
            case fields of
                (date:hTeam:hGoals:aTeam:aGoals:_) -> return (Fixture (Result day hTeam (read hGoals) aTeam (read aGoals)))
                                                      -- RLT dates are 8-character strings in DDMMYYYY format.
                                                      where day = readTime defaultTimeLocale "%d%m%Y" date
                ("AWARDED":team:points:[])         -> return (Adjustment team (read points))
                ("DEDUCTED":team:points:[])        -> return (Adjustment team (-read points))
                otherwise                          -> return (Metadata fields)

field = many1 (noneOf "|\n")

-- | A comment starts with a hash and continues to the end of the line.
comment :: Parser Item
comment = do char '#'
             text <- manyTill anyChar newline
             return (Comment text)

-- | Takes a list of parsed items and discards comments and meta-data.  The remaining items are separated into a list
--   of results and a map of net points adjustments by team.
extractData :: [Item] -> ([Result], Map Team Int) -> ([Result], Map Team Int)
extractData [] (r, a)                             = (sort r, a) -- Sort results as the final operation because otherwise they are backwards.
extractData (Fixture result:items) (r, a)         = extractData items (result:r, a)
extractData (Adjustment team amount:items) (r, a) = extractData items (r, addAdjustment team amount a)
extractData (_:items) (r, a)                      = extractData items (r, a)

-- | Adds a points adjustment to a map of existing points adjustments.  All adjustments for an individual team are
--   collated into a single entry.
addAdjustment :: Team -> Int -> Map Team Int -> Map Team Int
addAdjustment team amount  map = Map.insert team total map
                                 where total = amount + Map.findWithDefault 0 team map 

-- | Parses the given string (the content of an RLT file) and returns a list of football results.
parseRLT :: String -> Either ParseError ([Result], Map Team Int)
parseRLT input = parse results "(unknown)" input

