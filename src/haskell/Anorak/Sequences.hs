-- | Functions for calculating sequences.
module Anorak.Sequences (getSequenceTables) where

import Anorak.Results
import Data.List(foldl', sortBy)
import Data.Map(Map, (!))
import qualified Data.Map as Map(fromList, map, mapWithKey, toList)
import Data.Ord(comparing)
import Data.Sequence(Seq, (|>))
import qualified Data.Sequence as Seq(empty, length, null)

-- | The supported sequence types.
data SequenceType = Wins | Draws | Losses | Unbeaten | NoWin | Cleansheets | Conceded | Scored | NoGoal
    deriving (Eq, Ord, Show)

-- | Team sequences consist of a map from SequenceType to a sequence of results.
type TeamSequences = Map SequenceType (Seq TeamResult)
-- | Combined team sequences maps SequenceType to both the current and longest sequence.
type CombinedTeamSequences = Map SequenceType (Seq TeamResult, Seq TeamResult)
-- | A Sequence table is a list of teams and their associated sequence of results.
type SequenceTable = [(Team, Seq TeamResult)]

-- | Given a map of results by team, calculate the current and longest sequences for each team.
getSequenceTables :: Map Team [Result] -> (Map SequenceType SequenceTable, Map SequenceType SequenceTable)
getSequenceTables results = (sequenceTables $ Map.map (Map.map fst) teamSequences, sequenceTables $ Map.map (Map.map snd) teamSequences)
                            where teamSequences = sequencesByTeam results

sequenceTables :: Map Team TeamSequences -> Map SequenceType SequenceTable
sequenceTables seqs = Map.fromList [(Wins, sequenceTable seqs Wins),
                                    (Draws, sequenceTable seqs Draws),
                                    (Losses, sequenceTable seqs Losses),
                                    (Unbeaten, sequenceTable seqs Unbeaten),
                                    (NoWin, sequenceTable seqs NoWin),
                                    (Cleansheets, sequenceTable seqs Cleansheets),
                                    (Conceded, sequenceTable seqs Conceded),
                                    (Scored, sequenceTable seqs Scored),        
                                    (NoGoal, sequenceTable seqs NoGoal)]

sequenceTable :: Map Team TeamSequences -> SequenceType -> SequenceTable
sequenceTable seqs seqType = sortTable $ Map.toList (Map.map (! seqType) seqs)

-- | Sort a sequence table.
sortTable :: SequenceTable -> SequenceTable
sortTable = sortBy compareSequence . filter (not.Seq.null.snd)

-- | Comparator for a list of sequences, longest first.
compareSequence :: (Team, Seq TeamResult) -> (Team, Seq TeamResult) -> Ordering
compareSequence (t1, s1) (t2, s2)
    | Seq.length s1 == Seq.length s2 = compare t1 t2
    | otherwise                      = comparing Seq.length s2 s1

-- | Calculate current and longest sequences for each team.
sequencesByTeam :: Map Team [Result] -> Map Team CombinedTeamSequences
sequencesByTeam results = Map.map sequences teamResults
                          where teamResults = Map.mapWithKey (map . convertResult) results

-- | Calculate current and longest sequences for a team given their results.
sequences :: [TeamResult] -> CombinedTeamSequences
sequences = foldl' addResultToAllSequences emptySequences

-- | Create an empty CombinedTeamSequences structure.
emptySequences:: CombinedTeamSequences
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
addResultToAllSequences :: CombinedTeamSequences -> TeamResult -> CombinedTeamSequences
addResultToAllSequences seqs result = Map.mapWithKey (addResultToSequences result) seqs

addResultToSequences :: TeamResult -> SequenceType -> (Seq TeamResult, Seq TeamResult) -> (Seq TeamResult, Seq TeamResult)
addResultToSequences result Wins seqs        = addMatchingResultToSequences result (('W' ==) . outcome) seqs 
addResultToSequences result Draws seqs       = addMatchingResultToSequences result (('D' ==) . outcome) seqs 
addResultToSequences result Losses seqs      = addMatchingResultToSequences result (('L' ==) . outcome) seqs 
addResultToSequences result Unbeaten seqs    = addMatchingResultToSequences result (('L' /=) . outcome) seqs 
addResultToSequences result NoWin seqs       = addMatchingResultToSequences result (('W' /=) . outcome) seqs 
addResultToSequences result Cleansheets seqs = addMatchingResultToSequences result ((0 ==) . conceded) seqs 
addResultToSequences result Conceded seqs    = addMatchingResultToSequences result ((0 <) . conceded) seqs 
addResultToSequences result Scored seqs      = addMatchingResultToSequences result ((0 <) . scored) seqs 
addResultToSequences result NoGoal seqs      = addMatchingResultToSequences result ((0 ==) . scored) seqs 

addMatchingResultToSequences :: TeamResult -> (TeamResult -> Bool) -> (Seq TeamResult, Seq TeamResult) -> (Seq TeamResult, Seq TeamResult)
addMatchingResultToSequences result predicate (current, overall)
    | predicate result = (updated, if Seq.length updated > Seq.length overall then updated else overall)
    | otherwise        = (Seq.empty, overall)
    where updated = current |> result

