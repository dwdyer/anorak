-- | Main module for the Anorak system.
module Anorak (Team, Result(Result), parseRLTFile) where

import Anorak.Types
import Anorak.RLTParser
import Data.Map(Map)
import Text.ParserCombinators.Parsec(ParseError)
import System(getArgs)

-- | Builds a LeagueRecord for the specified team, including all of the results (from those provided) in which that
--   team was involved.
buildRecord :: [Result] -> Team -> LeagueRecord
buildRecord results team = foldl (addResultToRecord team) (LeagueRecord team 0 0 0 0 0) results

addResultToRecord :: Team -> LeagueRecord -> Result -> LeagueRecord
addResultToRecord team record result 
    | team == (homeTeam result) = addScoreToRecord record (homeGoals result) (awayGoals result)
    | team == (awayTeam result) = addScoreToRecord record (awayGoals result) (homeGoals result)
    | otherwise                 = record

addScoreToRecord :: LeagueRecord -> Int -> Int -> LeagueRecord
addScoreToRecord (LeagueRecord team won drawn lost for against) scored conceded
    | scored > conceded  = (LeagueRecord team (won + 1) drawn lost (for + scored) (against + conceded))
    | scored == conceded = (LeagueRecord team won (drawn + 1) lost (for + scored) (against + conceded))
    | otherwise          = (LeagueRecord team won drawn (lost + 1) (for + scored) (against + conceded))

main :: IO()
main = do file:_ <- getArgs
          (teams, results, adjustments) <- parseRLTFile file
          print teams
