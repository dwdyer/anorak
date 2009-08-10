-- | Functions for calculating sequences.
module Anorak.Sequences (getSequences, sequenceTables) where

import Anorak.Types
import Data.Map(Map, (!))
import qualified Data.Map as Map(fromList, map, mapWithKey, toList)
import Data.Sequence(Seq, (|>))
import qualified Data.Sequence as Seq(empty, length, null)
import List(sortBy)

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

-- | Calculate current and longest sequences for each team.
sequencesByTeam :: Map Team [Result] -> Map Team TeamSequences
sequencesByTeam results = Map.map sequences teamResults
                          where teamResults = (Map.mapWithKey (\t rs -> map (convertResult t) rs) results)

-- | Calculate current and longest sequences for a team given their results.
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

-- | Adds a result to any sequences that it relates to.
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

