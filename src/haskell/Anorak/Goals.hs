module Anorak.Goals (topGoalScorers) where

import Anorak.Results
import Data.List(groupBy, sortBy)
import Data.Ord(comparing)

-- | Generate a list of all goal scorers for a given set of results.
topGoalScorers :: [Result] -> [(Team, (String, Int))]
topGoalScorers results = sortBy (\(_, (_, g1)) (_, (_, g2)) -> compare g2 g1) $ map (\s -> (snd $ head s, (scorer $ fst $ head s, length s))) byScorer
                         where goals = foldl (\gs r -> gs ++ extractGoals r) [] results -- Extract all goals from the list of results, gives list of (Goal, Team).
                               noOGs = sortBy (\g h -> comparing scorer (fst g) (fst h)) $ filter (\g -> goalType (fst g) /= "o") goals -- Remove own goals from list and sort by scorer.
                               byScorer = groupBy (\g h -> scorer (fst g) == scorer (fst h)) noOGs

extractGoals :: Result -> [(Goal, Team)]
extractGoals result = getGoals homeTeam homeGoals ++ getGoals awayTeam awayGoals
                      where getGoals ft fg = map (\g -> (g, ft result)) $ fg result
