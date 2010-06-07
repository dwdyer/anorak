-- | Functions for generating league tables (including form tables and mini-leagues).
module Anorak.Tables (buildRecord, formTable, goalDiff, LeagueRecord(..), leagueTable, miniLeagueTable, played, points, pointsPerGame) where

import Anorak.Results
import Anorak.Utils(keep)
import qualified Data.ByteString.Char8 as BS(unpack)
import Data.List(foldl', sort)
import Data.Map(Map, (!))
import qualified Data.Map as Map(elems, empty, filterWithKey, findWithDefault, map, mapWithKey)
import Data.Ord(comparing)
import Data.Set(Set)
import qualified Data.Set as Set(member)

-- | A LeagueRecord contains data about the league performance of a single team.  It
--   includes total number of wins, draws, defeats, goals scored and goals conceded.
data LeagueRecord = LeagueRecord {team :: !Team,                       -- ^ The team that this record relates to.
                                  won :: !Int,                         -- ^ The number of matches won by this team.
                                  drawn :: !Int,                       -- ^ The number of matches drawn by this team.
                                  lost :: !Int,                        -- ^ The number of matches lost by this team.
                                  for :: !Int,                         -- ^ The total number of goals scored by this team.
                                  against :: !Int,                     -- ^ The total number of goals conceded by this team.
                                  adjustment :: !Int                   -- ^ A points adjustment (can be positive or negative but is usually zero) to be applied to this team's total.
                                 }
-- A LeagueRecord can be rendered as a String containing both member fields and
-- derived fields.
instance Show LeagueRecord where
    show record = BS.unpack (team record) ++
                  " P" ++ show (played record) ++
                  " W" ++ show (won record) ++
                  " D" ++ show (drawn record) ++
                  " L" ++ show (lost record) ++
                  " F" ++ show (for record) ++
                  " A" ++ show (against record) ++
                  " GD" ++ show (goalDiff record) ++
                  " Pts" ++ show (points record)
-- A LeagueRecord can be compared with other records to provide an ordering for a
-- list of records.
instance Eq LeagueRecord where
    (==) record1 record2 = points record1 == points record2
                           && goalDiff record1 == goalDiff record2
                           && for record1 == for record2
                           && won record1 == won record2
instance Ord LeagueRecord where
    compare record1 record2
        | points record1 /= points record2     = comparing points record2 record1
        | goalDiff record1 /= goalDiff record2 = comparing goalDiff record2 record1
        | for record1 /= for record2           = comparing for record2 record1
        | won record1 /= won record2           = comparing won record2 record1
        | otherwise                            = EQ

-- | Calculates the total number of matches played (the sum or wins, draws and defeats).
played :: LeagueRecord -> Int
played record = won record + drawn record + lost record

-- | Calculates the total number of points (3 points for each win, 1 for each draw, +/- any adjustment).
points :: LeagueRecord -> Int
points record = won record * 3 + drawn record + adjustment record

-- | Calculates goal difference (total scored minus total conceded)
goalDiff :: LeagueRecord -> Int
goalDiff record = for record - against record

-- | Calculates average number of league points earned per game.
pointsPerGame :: LeagueRecord -> Double
pointsPerGame record = fromIntegral (points record) / fromIntegral (played record)

-- | Builds a LeagueRecord for the specified team, including all of the results (from those provided) in which that
--   team was involved.
buildRecord :: Team -> [Result] -> LeagueRecord
buildRecord team = foldl' (addResultToRecord team) (LeagueRecord team 0 0 0 0 0 0)

-- | Adds a single match result to a particular team's league record.  If the specified team was not involved in that
--   match, the match is ignored.
addResultToRecord :: Team -> LeagueRecord -> Result -> LeagueRecord
addResultToRecord team record result 
    | team == homeTeam result = addScoreToRecord record (homeScore result) (awayScore result)
    | team == awayTeam result = addScoreToRecord record (awayScore result) (homeScore result)
    | otherwise               = record

addScoreToRecord :: LeagueRecord -> Int -> Int -> LeagueRecord
addScoreToRecord (LeagueRecord team won drawn lost for against adjustment) scored conceded
    | scored > conceded  = LeagueRecord team (won + 1) drawn lost (for + scored) (against + conceded) adjustment
    | scored == conceded = LeagueRecord team won (drawn + 1) lost (for + scored) (against + conceded) adjustment
    | otherwise          = LeagueRecord team won drawn (lost + 1) (for + scored) (against + conceded) adjustment

-- | Produces a standard league table with teams ordered in descending order of points.  Takes a map of teams to
--   results and a map of points adjustments and returns a sorted list of league records.
leagueTable :: Map Team [Result] -> Map Team Int -> Int -> [LeagueRecord]
leagueTable teamResults adjustments 0     = sort $ map (adjust adjustments) table
                                            where table = Map.elems $ Map.mapWithKey buildRecord teamResults
leagueTable teamResults adjustments split = updateSection top remainingFixtures ++ updateSection bottom remainingFixtures
                                            where before = leagueTable (Map.map (take split) teamResults) adjustments 0 -- Initial table based on early fixtures.
                                                  (top, bottom) = splitAt (length before `div` 2) before -- Split league in half.
                                                  remainingFixtures = Map.map (drop split) teamResults -- Keep remaining fixtures and apply to both halves before recombining.
                                           

-- | Update the specified table by applying remaining results for member teams.
updateSection :: [LeagueRecord] -> Map Team [Result] -> [LeagueRecord]
updateSection table results = sort $ map (updateRecord results) table

-- | Update the specified league record by applying all of the remaining results for that team.
updateRecord :: Map Team [Result] -> LeagueRecord -> LeagueRecord
updateRecord results record = foldl' (addResultToRecord t) record (results ! t)
                              where t = team record

-- | Produces a form table with teams ordered in descending order of points.
formTable :: Map Team [Result] -> Int -> [(LeagueRecord, [TeamResult])]
formTable teamResults n = map (attachForm formResults) $ leagueTable formResults Map.empty 0
                          where formResults = Map.map (keep n) teamResults

attachForm :: Map Team [Result] -> LeagueRecord -> (LeagueRecord, [TeamResult])
attachForm results record = (record, map (convertResult (team record)) formResults)
                            where formResults = Map.findWithDefault [] (team record) results

-- | Looks up the points adjustment for a team (if any) and applies it to their league record.
adjust :: Map Team Int -> LeagueRecord -> LeagueRecord
adjust adjustments (LeagueRecord t w d l f a adj) = LeagueRecord t w d l f a (adj + Map.findWithDefault 0 t adjustments)

-- | Generate a league table that includes only results between the specified teams.
miniLeagueTable :: Set Team -> Map Team [Result] -> [LeagueRecord]
miniLeagueTable teams results = leagueTable filteredResults Map.empty 0
                                where filteredResults = Map.map (filter $ checkBothTeamsInSet teams) $ Map.filterWithKey (\k _ -> Set.member k teams) results

checkBothTeamsInSet :: Set Team -> Result -> Bool
checkBothTeamsInSet teams result = Set.member (homeTeam result) teams && Set.member (awayTeam result) teams

