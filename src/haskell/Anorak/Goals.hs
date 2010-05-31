module Anorak.Goals (topGoalScorers) where

import Anorak.Results
import Data.List(foldl', groupBy, sortBy)
import Data.Ord(comparing)

-- | Generate a list of all goal scorers for a given set of results.
topGoalScorers :: [Result] -> [(Team, (String, Int))]
topGoalScorers results = sortBy (comparing $ snd.snd) $ map (\s -> (snd $ head s, (scorer.fst $ head s, length s))) byScorer
                         where goals = foldl' (flip $ (++).extractGoals) [] results -- Extract all goals from the list of results, gives list of (Goal, Team).
                               noOGs = sortBy (comparing $ scorer.fst) $ filter ((/=) "o".goalType.fst) goals -- Remove own goals from list and sort by scorer.
                               byScorer = groupBy (\g h -> scorer (fst g) == scorer (fst h)) noOGs

extractGoals :: Result -> [(Goal, Team)]
extractGoals result = getGoals homeTeam homeGoals ++ getGoals awayTeam awayGoals
                      where getGoals ft fg = map (\g -> (g, ft result)) $ fg result
