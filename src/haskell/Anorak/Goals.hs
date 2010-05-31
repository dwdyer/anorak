module Anorak.Goals (teamGoalScorers, topGoalScorers) where

import Anorak.Results
import Data.List(foldl', groupBy, sortBy)
import Data.Ord(comparing)

-- | Generate a list of all goal scorers for a given set of results.
topGoalScorers :: [Result] -> [(Team, (String, Int))]
topGoalScorers results = reverse . sortBy (comparing $ snd.snd) $ map (\s -> (snd $ head s, (scorer.fst $ head s, length s))) byScorer
                         where goals = concatMap extractGoals results
                               byScorer = groupByScorer (scorer.fst) $ filter ((/=) "o".goalType.fst) goals -- Omit own goals and group by scorer.

extractGoals :: Result -> [(Goal, Team)]
extractGoals result = getGoals homeTeam homeGoals ++ getGoals awayTeam awayGoals
                      where getGoals ft fg = map (\g -> (g, ft result)) $ fg result

-- | Sort and group by scorer.  The type g could be either Goal or (Goal, Team), so the first argument
--   is the function to extract the scorer.
groupByScorer :: (g -> String) -> [g] -> [[g]]
groupByScorer fs = groupBy (\a b -> fs a == fs b) . sortBy (comparing fs)

-- | Generate a list of goal scorers for a particular team.
teamGoalScorers :: [TeamResult] -> [(String, Int)]
teamGoalScorers results = reverse . sortBy (comparing snd) $ map (\s -> (scorer $ head s, length s)) byScorer
                          where teamGoals = concatMap goals results
                                byScorer = groupByScorer scorer $ filter ((/=) "o".goalType) teamGoals -- Omit own goals and group by scorer.

