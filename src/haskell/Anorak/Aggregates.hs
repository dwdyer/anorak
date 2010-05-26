-- | Functions for calculating aggregates (total cleansheets, total goal-less draws, etc.)
module Anorak.Aggregates (getAggregateTables) where

import Anorak.Results
import Data.Ord(comparing)
import Data.Map(Map, (!))
import qualified Data.Map as Map(fromList, map, mapWithKey, toList)
import List(sortBy)

-- | The supported aggregate types.
data AggregateType = Cleansheets | Blanks | BoreDraws | ScoreDraws
    deriving (Eq, Ord, Show)

-- | Given a map of results by team, generate a map of aggregate tables, keyed by aggregate type.
getAggregateTables :: Map Team [Result] -> Map AggregateType [(Team, Int)]
getAggregateTables results = Map.fromList [(Cleansheets, aggregateTable teamAggregates Cleansheets),
                                           (Blanks, aggregateTable teamAggregates Blanks),
                                           (BoreDraws, aggregateTable teamAggregates BoreDraws),
                                           (ScoreDraws, aggregateTable teamAggregates Cleansheets)]
                             where teamAggregates = aggregatesByTeam results

aggregateTable :: Map Team (Map AggregateType Int) -> AggregateType -> [(Team, Int)]
aggregateTable aggregates aggType = sortTable $ Map.toList (Map.map (flip (!) aggType) aggregates)

-- | Sort aggregate table in descending order.
sortTable :: [(Team, Int)] -> [(Team, Int)]
sortTable = sortBy compareAggregate . filter ((/= 0).snd)

compareAggregate :: (Team, Int) -> (Team, Int) -> Ordering
compareAggregate (t1, n1) (t2, n2)
    | n1 == n2  = compare t1 t2
    | otherwise = compare n2 n1

-- | Calculate totals for each team.
aggregatesByTeam :: Map Team [Result] -> Map Team (Map AggregateType Int)
aggregatesByTeam results = Map.map aggregates teamResults
                           where teamResults = Map.mapWithKey (map . convertResult) results

-- | Given a list of results for a particular team, generate a map of aggregates.
aggregates :: [TeamResult] -> Map AggregateType Int
aggregates = foldl addResultToAllAggregates $ Map.fromList [(Cleansheets, 0), (Blanks, 0), (BoreDraws, 0), (ScoreDraws, 0)]

addResultToAllAggregates :: Map AggregateType Int -> TeamResult -> Map AggregateType Int
addResultToAllAggregates aggregates result = Map.mapWithKey (addResultToAggregates result) aggregates

addResultToAggregates :: TeamResult -> AggregateType -> Int -> Int
addResultToAggregates result Cleansheets n = if conceded result == 0 then n + 1 else n
addResultToAggregates result Blanks n      = if scored result == 0 then n + 1 else n
addResultToAggregates result BoreDraws n   = if conceded result == 0 && scored result == 0 then n + 1 else n
addResultToAggregates result ScoreDraws n  = if conceded result > 0 && scored result > 0 then n + 1 else n

