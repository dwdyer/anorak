-- | Core functionality for the Anorak system.
module Anorak.Core (leagueTable, resultsByDate, resultsByTeam, splitHomeAndAway) where

import Anorak.Types
import Data.Map(Map)
import Data.Time.Calendar(Day)
import qualified Data.Map as Map(elems, findWithDefault, insertWith, mapWithKey)
import List(partition, sort)

-- | Builds a LeagueRecord for the specified team, including all of the results (from those provided) in which that
--   team was involved.
buildRecord :: Team -> [Result] -> LeagueRecord
buildRecord team results = foldl (addResultToRecord team) (LeagueRecord team 0 0 0 0 0 0) results

-- | Adds a single match result to a particular team's league record.  If the specified team was not involved in that
--   match, the match is ignored.
addResultToRecord :: Team -> LeagueRecord -> Result -> LeagueRecord
addResultToRecord team record result 
    | team == (homeTeam result) = addScoreToRecord record (homeGoals result) (awayGoals result)
    | team == (awayTeam result) = addScoreToRecord record (awayGoals result) (homeGoals result)
    | otherwise                 = record

addScoreToRecord :: LeagueRecord -> Int -> Int -> LeagueRecord
addScoreToRecord (LeagueRecord team won drawn lost for against adjustment) scored conceded
    | scored > conceded  = (LeagueRecord team (won + 1) drawn lost (for + scored) (against + conceded) adjustment)
    | scored == conceded = (LeagueRecord team won (drawn + 1) lost (for + scored) (against + conceded) adjustment)
    | otherwise          = (LeagueRecord team won drawn (lost + 1) (for + scored) (against + conceded) adjustment)

-- | Convert a flat list of results into a mapping from team to list of results that that team was involved in.
resultsByTeam :: [Result] -> Map Team [Result] -> Map Team [Result]
resultsByTeam [] map          = map
resultsByTeam (result:rs) map = resultsByTeam rs (addResultToMap (addResultToMap map homeTeam result) awayTeam result)

-- | Convert a flat list of results into a mapping from date to list of matches played on that date.
resultsByDate :: [Result] -> Map Day [Result] -> Map Day [Result]
resultsByDate [] map          = map
resultsByDate (result:rs) map = resultsByDate rs (addResultToMap map date result)

-- | Helper function for adding a result to a map that maps an aribtrary key type to a list of results.
addResultToMap :: Ord k => Map k [Result] -> (Result -> k) -> Result -> Map k [Result]
addResultToMap map keyFunction result = Map.insertWith (flip (++)) (keyFunction result) [result] map

-- | Splits each team's results into home results and away results.  The first item in the mapped tuple is the team's
--   home results, the second is their away results.
splitHomeAndAway :: Map Team [Result] -> Map Team ([Result], [Result])
splitHomeAndAway results = Map.mapWithKey partitionResults results

-- | Splits a team's results into home results and away results.  The first item in the tuple is the home results, the
--   second is the away results.
partitionResults :: Team -> [Result] -> ([Result], [Result])
partitionResults team results = partition (\x -> team == homeTeam x) results

-- | Produces a standard league table with teams ordered in descending order of points.  Takes a map of teams to
--   results and a map of points adjustments and returns a sorted list of league records.
leagueTable :: Map Team [Result] -> Map Team Int -> [LeagueRecord]
leagueTable teamResults adjustments  = sort $ map (adjust adjustments) table
                                       where table = Map.elems $ Map.mapWithKey (buildRecord) teamResults

-- | Looks up the points adjustment for a team (if any) and applies it to their league record.
adjust :: Map Team Int -> LeagueRecord -> LeagueRecord
adjust adjustments (LeagueRecord t w d l f a adj) = (LeagueRecord t w d l f a (adj + Map.findWithDefault 0 t adjustments))