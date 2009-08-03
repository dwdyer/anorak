-- | Core functionality for the Anorak system.
module Anorak.Core (formTable, leagueTable, resultsByDate, resultsByTeam, sequenceTable, splitHomeAndAway) where

import Anorak.Types
import Data.Map(Map)
import qualified Data.Map as Map(assocs, elems, empty, findWithDefault, insertWith, map, mapWithKey)
import Data.Sequence(Seq, (|>))
import qualified Data.Sequence as Seq(empty, length, null)
import Data.Time.Calendar(Day)
import List(partition, sort, sortBy)

-- | Builds a LeagueRecord for the specified team, including all of the results (from those provided) in which that
--   team was involved.
buildRecord :: Team -> [Result] -> LeagueRecord
buildRecord team results = foldl (addResultToRecord team) (emptyRecord team) results

-- | Returns a blank team record.
emptyRecord :: Team -> LeagueRecord
emptyRecord team = LeagueRecord team 0 0 0 0 0 0

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

-- | Returns the current and overall sequence of a given type for all teams.
sequenceTable :: Map Team [Result] -> SequenceType -> ([(Team, Seq TeamResult)], [(Team, Seq TeamResult)])
sequenceTable results seqType = let (current, overall) = (Map.assocs $ Map.map (fst) teamSequences, Map.assocs $ Map.map (snd) teamSequences)
                                in (sortSequences current, sortSequences overall)
                                where teamSequences = sequencesByTeam results seqType

sortSequences :: [(Team, Seq TeamResult)] -> [(Team, Seq TeamResult)]
sortSequences seq = sortBy compareSequence (filter (not.Seq.null.snd) seq)

compareSequence :: (Team, Seq TeamResult) -> (Team, Seq TeamResult) -> Ordering
compareSequence (t1, s1) (t2, s2)
    | Seq.length s1 == Seq.length s2 = compare t1 t2
    | otherwise                      = compare (Seq.length s2) (Seq.length s1)

sequencesByTeam :: Map Team [Result] -> SequenceType -> Map Team (Seq TeamResult, Seq TeamResult)
sequencesByTeam results seqType = Map.map (sequences seqType) teamResults
                                  where teamResults = (Map.mapWithKey (\t rs -> map (convertResult t) rs) results)

sequences :: SequenceType -> [TeamResult] -> (Seq TeamResult, Seq TeamResult)
sequences seqType results = foldl (addResultToSequences seqType) (Seq.empty, Seq.empty) results

addResultToSequences :: SequenceType -> (Seq TeamResult, Seq TeamResult) -> TeamResult -> (Seq TeamResult, Seq TeamResult)
addResultToSequences Wins sequences result        = addMatchingResultToSequences result (\r -> outcome r == 'W') sequences 
addResultToSequences Draws sequences result       = addMatchingResultToSequences result (\r -> outcome r == 'D') sequences 
addResultToSequences Losses sequences result      = addMatchingResultToSequences result (\r -> outcome r == 'L') sequences 
addResultToSequences Unbeaten sequences result    = addMatchingResultToSequences result (\r -> outcome r /= 'L') sequences 
addResultToSequences NoWin sequences result       = addMatchingResultToSequences result (\r -> outcome r /= 'W') sequences 
addResultToSequences Cleansheets sequences result = addMatchingResultToSequences result (\r -> conceded r == 0) sequences 
addResultToSequences Scored sequences result      = addMatchingResultToSequences result (\r -> scored r > 0) sequences 
addResultToSequences NoGoal sequences result      = addMatchingResultToSequences result (\r -> scored r == 0) sequences 

addMatchingResultToSequences :: TeamResult -> (TeamResult -> Bool) -> (Seq TeamResult, Seq TeamResult) -> (Seq TeamResult, Seq TeamResult)
addMatchingResultToSequences result predicate (current, overall)
    | (predicate result) = (updated, if Seq.length updated > Seq.length overall then updated else overall)
    | otherwise          = (Seq.empty, overall)
    where updated = current |> result
