-- | A module for passing RLT files as used by the Football Statistics Applet <https://fsa.dev.java.net>
module RLTParser (Team, Result, parseResults) where

import Text.ParserCombinators.Parsec

-- | A team is represented simply by its name.
type Team = String

-- | A match result consists of two teams and the goals scored by each.
data Result = Result {homeTeam :: Team,
                      homeGoals :: Int,
                      awayTeam :: Team,
                      awayGoals :: Int}
instance Show Result where
    show result = homeTeam result ++ " " ++
                  show (homeGoals result) ++ " - " ++
                  show (awayGoals result) ++ " " ++
                  awayTeam result

-- | An RLT file consists of many items (results, metadata and comments).
data Item = Fixture Result    -- ^ The result of a single football match.
          | Metadata [String] -- ^ Data about the league, such as which positions are promoted or relegated.
          | Comment String    -- ^ Comments about the data.
    deriving Show

-- | Parse results, discard meta-data and comments.
results :: Parser [Result]
results = do list <- items
             return (keepResults list)

-- | Each line of a data file is either a record (a match result or metadata) or a comment.
items :: Parser [Item]
items = many (comment <|> record)

-- | A record is a list of fields delimited by pipe characters.
record :: Parser Item
record = do fields <- sepBy field (char '|')
            newline
            case fields of
                (date:hTeam:hGoals:aTeam:aGoals:_) -> return (Fixture (Result hTeam (read hGoals) aTeam (read aGoals)))
                otherwise                          -> return (Metadata fields)

field = many (noneOf "|\n")

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

