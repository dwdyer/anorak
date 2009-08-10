-- | Core functionality for the Anorak system.
module Anorak.Core (formTable, getSequences, leagueTable, miniLeagueTable, resultsByDate, resultsByTeam, sequenceTables, splitHomeAndAway) where

import Anorak.Types
import Data.Map(Map, (!))
import qualified Data.Map as Map(assocs, elems, empty, filterWithKey, findWithDefault, fromList, insertWith, map, mapWithKey, toList)
import Data.Sequence(Seq, (|>))
import qualified Data.Sequence as Seq(empty, length, null)
import Data.Set(Set)
import qualified Data.Set as Set(member)
import Data.Time.Calendar(Day)
import List(partition, sort, sortBy)

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
adjust adjustments (LeagueRecord t w d l f a adj) = (LeagueRecord t w d l f a (adj + Map.findWithDefault 0 t adjustments))

-- | Converts a Result into a TeamResult for the specified team.
convertResult :: Team -> Result -> TeamResult
convertResult team result
    | team == (homeTeam result) = (TeamResult (date result) (awayTeam result) 'H' (homeGoals result) (awayGoals result) (form team result))
    | otherwise                 = (TeamResult (date result) (homeTeam result) 'A' (awayGoals result) (homeGoals result) (form team result))

sequenceTables :: Map Team (Map SequenceType (Seq TeamResult)) -> Map String [(Team, Seq TeamResult)]
sequenceTables sequences = Map.fromList [(show Wins, sequenceTable sequences Wins),
                                         (show Draws, sequenceTable sequences Draws),
                                         (show Losses, sequenceTable sequences Losses),
                                         (show Unbeaten, sequenceTable sequences Unbeaten),
                                         (show NoWin, sequenceTable sequences NoWin),
                                         (show Cleansheets, sequenceTable sequences Cleansheets),
                                         (show Conceded, sequenceTable sequences Conceded),
                                         (show Scored, sequenceTable sequences Scored),        
                                         (show NoGoal, sequenceTable sequences NoGoal)]

sequenceTable :: Map Team (Map SequenceType (Seq TeamResult)) -> SequenceType -> [(Team, Seq TeamResult)]
sequenceTable sequences seqType = sortSequences $ Map.toList (Map.map (flip (!) seqType) sequences)

-- | Sort a sequence table.
sortSequences :: [(Team, Seq TeamResult)] -> [(Team, Seq TeamResult)]
sortSequences seq = sortBy compareSequence (filter (not.Seq.null.snd) seq)

-- | Comparator for a list of sequences, longest first.
compareSequence :: (Team, Seq TeamResult) -> (Team, Seq TeamResult) -> Ordering
compareSequence (t1, s1) (t2, s2)
    | Seq.length s1 == Seq.length s2 = compare t1 t2
    | otherwise                      = compare (Seq.length s2) (Seq.length s1)

-- | Given a map of results by team, calculate the current and longest sequences for each team.
getSequences :: Map Team [Result] ->  (Map Team (Map SequenceType (Seq TeamResult)), Map Team (Map SequenceType (Seq TeamResult)))
getSequences results = (Map.map (Map.map fst) teamSequences, Map.map (Map.map snd) teamSequences)
                       where teamSequences = sequencesByTeam results

sequencesByTeam :: Map Team [Result] -> Map Team TeamSequences
sequencesByTeam results = Map.map sequences teamResults
                          where teamResults = (Map.mapWithKey (\t rs -> map (convertResult t) rs) results)

sequences :: [TeamResult] -> TeamSequences
sequences results = foldl addResultToAllSequences emptySequences results

-- | Create an empty TeamSequences structure.
emptySequences:: TeamSequences
emptySequences = Map.fromList [(Wins, (Seq.empty, Seq.empty)),
                               (Draws, (Seq.empty, Seq.empty)),
                               (Losses, (Seq.empty, Seq.empty)),
                               (Unbeaten, (Seq.empty, Seq.empty)),
                               (NoWin, (Seq.empty, Seq.empty)),
                               (Cleansheets, (Seq.empty, Seq.empty)),
                               (Conceded, (Seq.empty, Seq.empty)),
                               (Scored, (Seq.empty, Seq.empty)),
                               (NoGoal, (Seq.empty, Seq.empty))]

addResultToAllSequences :: TeamSequences -> TeamResult -> TeamSequences
addResultToAllSequences sequences result = Map.mapWithKey (addResultToSequences result) sequences

addResultToSequences :: TeamResult -> SequenceType -> (Seq TeamResult, Seq TeamResult) -> (Seq TeamResult, Seq TeamResult)
addResultToSequences result Wins sequences        = addMatchingResultToSequences result (\r -> outcome r == 'W') sequences 
addResultToSequences result Draws sequences       = addMatchingResultToSequences result (\r -> outcome r == 'D') sequences 
addResultToSequences result Losses sequences      = addMatchingResultToSequences result (\r -> outcome r == 'L') sequences 
addResultToSequences result Unbeaten sequences    = addMatchingResultToSequences result (\r -> outcome r /= 'L') sequences 
addResultToSequences result NoWin sequences       = addMatchingResultToSequences result (\r -> outcome r /= 'W') sequences 
addResultToSequences result Cleansheets sequences = addMatchingResultToSequences result (\r -> conceded r == 0) sequences 
addResultToSequences result Conceded sequences    = addMatchingResultToSequences result (\r -> conceded r > 0) sequences 
addResultToSequences result Scored sequences      = addMatchingResultToSequences result (\r -> scored r > 0) sequences 
addResultToSequences result NoGoal sequences      = addMatchingResultToSequences result (\r -> scored r == 0) sequences 

addMatchingResultToSequences :: TeamResult -> (TeamResult -> Bool) -> (Seq TeamResult, Seq TeamResult) -> (Seq TeamResult, Seq TeamResult)
addMatchingResultToSequences result predicate (current, overall)
    | (predicate result) = (updated, if Seq.length updated > Seq.length overall then updated else overall)
    | otherwise          = (Seq.empty, overall)
    where updated = current |> result

-- | Generate a league table that includes only results between the specified teams.
miniLeagueTable :: Set Team -> Map Team [Result] -> [LeagueRecord]
miniLeagueTable teams results = leagueTable filteredResults Map.empty
                                where filteredResults = Map.map (filter $ checkBothTeamsInSet $ teams) $ Map.filterWithKey (\k m -> Set.member k $ teams) results

checkBothTeamsInSet :: Set Team -> Result -> Bool
checkBothTeamsInSet teams result = Set.member (homeTeam result) teams && Set.member (awayTeam result) teams
