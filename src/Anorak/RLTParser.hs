-- | A module for parsing RLT data files (as used by the Football Statistics Applet <https://fsa.dev.java.net>).
module Anorak.RLTParser (parseResults) where

import Anorak.Types
import Data.Time.Calendar(Day)
import Data.Time.Format(readTime)
import List(sort)
import System.Locale(defaultTimeLocale)
import Text.ParserCombinators.Parsec

-- | An RLT file consists of many items (results, metadata and comments).
data Item = Fixture Result    -- ^ The result of a single football match.
          | Points Adjustment -- ^ A number of points awarded to or deducted from an individual team.
          | Metadata [String] -- ^ Data about the league, such as which positions are promoted or relegated.
          | Comment String    -- ^ Comments about the data.

-- | Parse results, discard meta-data and comments.
results :: Parser ([Result], [Adjustment])
results = do list <- items
             return (extractData list ([], []))

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
                ("AWARDED":team:points:[])         -> return (Points (Adjustment team (read points)))
                ("DEDUCTED":team:points:[])        -> return (Points (Adjustment team (-read points)))
                otherwise                          -> return (Metadata fields)

field = many1 (noneOf "|\n")

-- | A comment starts with a hash and continues to the end of the line.
comment :: Parser Item
comment = do char '#'
             text <- manyTill anyChar newline
             return (Comment text)

-- | Takes a list of parsed items and discards comments and meta-data.  The remaining items are divided
--   into two lists - match results and points adjustments.
extractData :: [Item] -> ([Result], [Adjustment]) -> ([Result], [Adjustment])
extractData [] (r, a)                        = (sort r, a) -- Sort results as the final operation because otherwise they are backwards.
extractData (Fixture result:items) (r, a)    = extractData items (result:r, a)
extractData (Points adjustment:items) (r, a) = extractData items (r, adjustment:a)
extractData (_:items) (r, a)                 = extractData items (r, a)

-- | Parses the given string (the content of an RLT file) and returns a list of football results.
parseResults :: String -> Either ParseError ([Result], [Adjustment])
parseResults input = parse results "(unknown)" input

