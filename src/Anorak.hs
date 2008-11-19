-- | Main module for the Anorak system.
module Anorak (Team, Result(Result), parseFile) where

import Anorak.Types
import Anorak.RLTParser
import Text.ParserCombinators.Parsec(ParseError)

parseFile :: FilePath -> IO (Either ParseError [Result])
parseFile path = do contents <- readFile path
                    return (parseResults contents)

