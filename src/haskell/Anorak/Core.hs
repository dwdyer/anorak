-- | Core functionality for the Anorak system.
module Anorak.Core (formTable, leagueTable, resultsByDate, resultsByTeam, splitHomeAndAway) where

import Anorak.Types
import Data.Map(Map)
import Data.Time.Calendar(Day)
import qualified Data.Map as Map(elems, empty, findWithDefault, insertWith, map, mapWithKey)
import List(partition, sort)

-- | Builds a LeagueRecord for the specified team, including all of the results (from those provided) in which that
--   team was involved.
buildRecord :: Team -> [Result] -> LeagueRecord
buildRecord team results = foldl (addResultToRecord team) (emptyRecord team) results

-- | Returns a blank team record.
emptyRecord :: Team -> LeagueRecord
emptyRecord team = LeagueRecord team 0 0 0 0 0 0 (Sequences 0 0 0 0 0 0 0 0, Sequences 0 0 0 0 0 0 0 0)

-- | Adds a single match result to a particular team's league record.  If the specified team was not involved in that
--   match, the match is ignored.
addResultToRecord :: Team -> LeagueRecord -> Result -> LeagueRecord
addResultToRecord team record result 
    | team == (homeTeam result) = addScoreToRecord record (homeGoals result) (awayGoals result)
    | team == (awayTeam result) = addScoreToRecord record (awayGoals result) (homeGoals result)
    | otherwise                 = record

addScoreToRecord :: LeagueRecord -> Int -> Int -> LeagueRecord
addScoreToRecord (LeagueRecord team won drawn lost for against adjustment sequences) scored conceded
    | scored > conceded  = (LeagueRecord team (won + 1) drawn lost (for + scored) (against + conceded) adjustment updatedSequences)
    | scored == conceded = (LeagueRecord team won (drawn + 1) lost (for + scored) (against + conceded) adjustment updatedSequences)
    | otherwise          = (LeagueRecord team won drawn (lost + 1) (for + scored) (against + conceded) adjustment updatedSequences)
    where updatedSequences = updateSequencesWithScore sequences scored conceded

-- | Updates the current and overall sequences with the specified score.
updateSequencesWithScore :: (Sequences, Sequences) -> Int -> Int -> (Sequences, Sequences)
updateSequencesWithScore (current, overall) scored conceded = (updated, maxSequences updated overall)
                                                              where updated = addScoreToSequences current scored conceded

-- | Updates a set of sequences with a new result.
addScoreToSequences :: Sequences -> Int -> Int -> Sequences
addScoreToSequences (Sequences wins draws losses unbeaten noWin cleansheets scoredGoal noGoal) scored conceded 
    | scored > conceded            = (Sequences (wins + 1) 0 0 (unbeaten + 1) 0 (if conceded == 0 then (cleansheets + 1) else 0) (scoredGoal + 1) 0)
    | scored == 0 && conceded == 0 = (Sequences 0 (draws + 1) 0 (unbeaten + 1) (noWin + 1) (cleansheets + 1) 0 (noGoal + 1))
    | scored == conceded           = (Sequences 0 (draws + 1) 0 (unbeaten + 1) (noWin + 1) 0 (scoredGoal + 1) 0)
    | otherwise                    = (Sequences 0 0 (losses + 1) 0 (noWin + 1) 0 (if scored > 0 then scoredGoal + 1 else 0) (if scored == 0 then noGoal + 1 else 0))

-- | Takes two sets of sequences and returns a new set containing the highest value for each sequence.
maxSequences :: Sequences -> Sequences -> Sequences
maxSequences (Sequences a b c d e f g h) (Sequences i j k l m n o p) = (Sequences (max a i) (max b j) (max c k) (max d l) (max e m) (max f n) (max g o) (max h p))

-- | Convert a flat list of results into a mapping from team to list of results that that team was involved in.
resultsByTeam :: [Result] -> Map Team [Result]
resultsByTeam []          = Map.empty
resultsByTeam (result:rs) = addResultToMap (addResultToMap (resultsByTeam rs) homeTeam result) awayTeam result

-- | Convert a flat list of results into a mapping from date to list of matches played on that date.
resultsByDate :: [Result] -> Map Day [Result]
resultsByDate []          = Map.empty
resultsByDate (result:rs) = addResultToMap (resultsByDate rs) date result

-- | Helper function for adding a result to a map that maps an aribtrary key type to a list of results.
addResultToMap :: Ord k => Map k [Result] -> (Result -> k) -> Result -> Map k [Result]
addResultToMap map keyFunction result = Map.insertWith (++) (keyFunction result) [result] map

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
leagueTable teamResults adjustments = sort $ map (adjust adjustments) table
                                      where table = Map.elems $ Map.mapWithKey (buildRecord) teamResults

-- | Produces a form table with teams ordered in descending order of points.
formTable :: Map Team [Result] -> Int -> [(LeagueRecord, [TeamResult])]
formTable teamResults n = map (attachForm formResults) $ leagueTable formResults Map.empty
                          where formResults = Map.map (keep n) teamResults

attachForm :: Map Team [Result] -> LeagueRecord -> (LeagueRecord, [TeamResult])
attachForm results record = (record, map (convertResult (team record)) formResults)
                            where formResults = Map.findWithDefault [] (team record) results

-- | Maps a single result for a particular team to a character ('W', 'D' or 'L').
form :: Team -> Result -> Char
form team result
    | (homeGoals result) == (awayGoals result)                            = 'D'
    | (homeTeam result == team && homeGoals result > awayGoals result)
      || (awayTeam result == team && awayGoals result > homeGoals result) = 'W'
    | otherwise                                                           = 'L'

-- | Retains the last n elements in a list.
keep :: Int -> [a] -> [a]
keep n x = drop ((length x) - n) x

-- | Looks up the points adjustment for a team (if any) and applies it to their league record.
adjust :: Map Team Int -> LeagueRecord -> LeagueRecord
adjust adjustments (LeagueRecord t w d l f a adj seq) = (LeagueRecord t w d l f a (adj + Map.findWithDefault 0 t adjustments) seq)

-- | Converts a Result into a TeamResult for the specified team.
convertResult :: Team -> Result -> TeamResult
convertResult team result
    | team == (homeTeam result) = (TeamResult (date result) (awayTeam result) 'H' (homeGoals result) (awayGoals result) (form team result))
    | otherwise                 = (TeamResult (date result) (homeTeam result) 'A' (awayGoals result) (homeGoals result) (form team result))

