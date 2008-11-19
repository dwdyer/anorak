-- | A module for parsing RLT data files (as used by the Football Statistics Applet <https://fsa.dev.java.net>).
module Anorak.RLTParser (parseResults) where

import Anorak.Types
import Data.Time.Calendar(Day)
import Data.Time.Format(readTime)
import System.Locale(defaultTimeLocale)
import Text.ParserCombinators.Parsec

-- | An RLT file consists of many items (results, metadata and comments).
data Item = Fixture Result    -- ^ The result of a single football match.
          | Points Adjustment -- ^ A number of points awarded to or deducted from an individual team.
          | Metadata [String] -- ^ Data about the league, such as which positions are promoted or relegated.
          | Comment String    -- ^ Comments about the data.

-- | Parse results, discard meta-data and comments.
results :: Parser [Result]
results = do list <- items
             return (keepResults list)

-- | Each line of a data file is either a record (match result or metadata) or it is a comment.
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

-- | Take a list of parsed items and discard comments and meta-data.
keepResults :: [Item] -> [Result]
keepResults []                  = []
keepResults (Fixture result:xs) = result:(keepResults xs)
keepResults (_:xs)              = keepResults xs

-- | Parses the given string (the content of an RLT file) and returns a list of football results.
parseResults :: String -> Either ParseError [Result]
parseResults input = parse results "(unknown)" input

