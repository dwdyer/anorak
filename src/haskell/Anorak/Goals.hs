{-# LANGUAGE OverloadedStrings #-}

module Anorak.Goals (teamGoalScorers, topGoalScorers) where

import Anorak.Results
import Anorak.Utils(equal, snd3, takeAtLeast)
import Data.ByteString.Char8(ByteString)
import Data.List(foldl', groupBy, nub, sortBy)
import Data.Ord(comparing)

-- | Generate a list of all goal scorers for a given set of results.
topGoalScorers :: [Result] -> [(ByteString, Int, [Team])]
topGoalScorers results = takeAtLeast 25 $ groupBy (equal snd3) scorers
                         where goals = concatMap extractGoals results
                               byScorer = groupByScorer (scorer.fst) $ filter ((/=) "o".goalType.fst) goals -- Omit own goals and group by scorer.
                               scorers = sortBy (comparing snd3) $ map (\s -> (scorer.fst $ head s, length s, nub $ map snd s)) byScorer

extractGoals :: Result -> [(Goal, Team)]
extractGoals result = getGoals homeTeam homeGoals ++ getGoals awayTeam awayGoals
                      where getGoals ft fg = map (\g -> (g, ft result)) $ fg result

-- | Sort and group by scorer.  The type g could be either Goal or (Goal, Team), so the first argument
--   is the function to extract the scorer.
groupByScorer :: (g -> ByteString) -> [g] -> [[g]]
groupByScorer scorerFunction = groupBy (equal scorerFunction) . sortBy (comparing scorerFunction)

-- | Generate a list of goal scorers for a particular team.
teamGoalScorers :: [TeamResult] -> [(ByteString, Int)]
teamGoalScorers results = sortBy (flip $ comparing snd) $ map (\s -> (scorer $ head s, length s)) byScorer
                          where teamGoals = concatMap goals results
                                byScorer = groupByScorer scorer $ filter ((/=) "o".goalType) teamGoals -- Omit own goals and group by scorer.

