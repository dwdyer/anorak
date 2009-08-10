-- | Functions for generating league tables (including form tables and mini-leagues).
module Anorak.Tables (formTable, leagueTable, miniLeagueTable) where

import Anorak.Types
import Data.Map(Map)
import qualified Data.Map as Map(elems, empty, filterWithKey, findWithDefault, map, mapWithKey)
import Data.Set(Set)
import qualified Data.Set as Set(member)
import List(sort)

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

-- | Produces a standard league table with teams ordered in descending order of points.  Takes a map of teams to
--   results and a map of points adjustments and returns a sorted list of league records.
leagueTable :: Map Team [Result] -> Map Team Int -> [LeagueRecord]
leagueTable teamResults adjustments = sort $ map (adjust adjustments) table
                                      where table = Map.elems $ Map.mapWithKey (buildRecord) teamResults

-- | Produces a form table with teams ordered in descending order of points.
formTable :: Map Team [Result] -> Int -> [(LeagueRecord, [TeamResult])]
formTable teamResults n = map (attachForm formResults) $ leagueTable formResults Map.empty
                          where formResults = Map.map (keep n) teamResults

attachForm :: Map Team [Result] -> LeagueRecord -> (LeagueRecord, [TeamResult])
attachForm results record = (record, map (convertResult (team record)) formResults)
                            where formResults = Map.findWithDefault [] (team record) results

-- | Retains the last n elements in a list.
keep :: Int -> [a] -> [a]
keep n x = drop ((length x) - n) x

-- | Looks up the points adjustment for a team (if any) and applies it to their league record.
adjust :: Map Team Int -> LeagueRecord -> LeagueRecord
adjust adjustments (LeagueRecord t w d l f a adj) = (LeagueRecord t w d l f a (adj + Map.findWithDefault 0 t adjustments))

-- | Generate a league table that includes only results between the specified teams.
miniLeagueTable :: Set Team -> Map Team [Result] -> [LeagueRecord]
miniLeagueTable teams results = leagueTable filteredResults Map.empty
                                where filteredResults = Map.map (filter $ checkBothTeamsInSet $ teams) $ Map.filterWithKey (\k m -> Set.member k $ teams) results

checkBothTeamsInSet :: Set Team -> Result -> Bool
checkBothTeamsInSet teams result = Set.member (homeTeam result) teams && Set.member (awayTeam result) teams

