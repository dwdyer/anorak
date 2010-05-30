module Anorak.Goals (hasScorers, topGoalScorers) where

import Anorak.Results
import List(groupBy, sortBy)

-- | Generate a list of all goal scorers for a given set of results.
topGoalScorers :: [Result] -> [(Team, (String, Int))]
topGoalScorers results = sortBy (\(_, (_, g1)) (_, (_, g2)) -> compare g2 g1) $ map (\s -> (snd $ s!!0, (scorer $ fst $ s!!0, length s))) byScorer
                         where goals = foldl (\gs r -> gs ++ extractGoals r) [] results -- Extract all goals from the list of results, gives list of (Goal, Team).
                               noOGs = sortBy (\g h -> compare (scorer $ fst g) (scorer $ fst h)) $ filter (\g -> (goalType $ fst g) /= "o") goals -- Remove own goals from list and sort by scorer.
                               byScorer = groupBy (\g h -> (scorer $ fst g) == (scorer $ fst h)) noOGs

extractGoals :: Result -> [(Goal, Team)]
extractGoals result = getGoals (homeTeam) (homeGoals) ++ getGoals (awayTeam) (awayGoals)
                      where getGoals ft fg = map (\g -> (g, ft result)) $ fg result

-- | Check to see whether there is any goal-scorer information in the data.
hasScorers :: [Result] -> Bool
hasScorers []     = False
hasScorers (r:rs) = (length $ homeGoals r) > 0 || (length $ awayGoals r) > 0 || hasScorers rs
