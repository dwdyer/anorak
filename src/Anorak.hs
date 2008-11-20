-- | Main module for the Anorak system.
module Anorak (Team, Result(Result), parseRLTFile) where

import Anorak.Types
import Anorak.RLTParser
import Data.Map(Map)
import Text.ParserCombinators.Parsec(ParseError)

parseRLTFile :: FilePath -> IO (Either ParseError ([Result], Map Team Int))
parseRLTFile path = do contents <- readFile path
                       return (parseRLT contents)

