{-# LANGUAGE OverloadedStrings #-}

module Anorak.Goals (goalsByInterval,
                     hatTricks,
                     teamGoalScorers,
                     topGoalScorers,
                     topPenaltyScorers) where

import Anorak.Results
import Data.ByteString.Char8(ByteString)
import Data.List(foldl', groupBy, nub, partition, sortBy)
import qualified Data.Map as Map(alter, elems, fromAscList)
import Data.Maybe(fromMaybe)
import Data.Ord(comparing)
import Util.List(equal, takeAtLeast)
import Util.Tuple(pair, snd3)

-- | Generate a list of the leading goal scorers for a given set of results.
topGoalScorers :: [Result] -> [(ByteString, Int, [Team])]
topGoalScorers results = topScorers results ((/=) "o".goalType) 30 

-- | Generate a list of the leading penalty scorers for a given set of results.
topPenaltyScorers :: [Result] -> [(ByteString, Int, [Team])]
topPenaltyScorers results = topScorers results ((==) "p".goalType) 10

-- | General function for finding the top scorers of the type of goal matched by the specified filter.
topScorers :: [Result] -> (Goal -> Bool) -> Int -> [(ByteString, Int, [Team])]
topScorers results filtr count = takeAtLeast count $ groupBy (equal snd3) scorers
                                 where goalsList = concatMap (extractGoals filtr) results -- Only include goals that match the filter.
                                       byScorer = groupByScorer (scorer.fst) goalsList
                                       scorers = sortBy (flip $ comparing snd3) $ map (\s -> (scorer.fst $ head s, length s, nub $ map snd s)) byScorer

extractGoals :: (Goal -> Bool) -> Result -> [(Goal, Team)]
extractGoals filtr result = getGoals homeTeam homeGoals ++ getGoals awayTeam awayGoals
                            where getGoals ft fg = map (\g -> (g, ft result)) . filter filtr $ fg result

-- | Sort and group by scorer.  The type g could be either Goal or (Goal, Team), so the first argument
--   is the function to extract the scorer.
groupByScorer :: (g -> ByteString) -> [g] -> [[g]]
groupByScorer scorerFunction = groupBy (equal scorerFunction) . sortBy (comparing scorerFunction)

-- | Generate a list of goal scorers for a particular team.  Also returns a number of own goals scored by the team's opponents.
teamGoalScorers :: [TeamResult] -> ([(ByteString, Int)], Int)
teamGoalScorers results = (goalScorers, length ownGoals)
                          where teamGoals = concatMap goalsFor results
                                (normalGoals, ownGoals) = partition ((/= "o").goalType) teamGoals
                                byScorer = groupByScorer scorer normalGoals
                                goalScorers = sortBy (flip $ comparing snd) $ map (pair (scorer.head) length) byScorer

-- | Get a list of all hat-tricks scored in the specified results.
hatTricks :: [Result] -> [(ByteString, Int, Team, TeamResult)]
hatTricks = sortBy compareHatTrick . concatMap getMatchHatTricks

getMatchHatTricks :: Result -> [(ByteString, Int, Team, TeamResult)]
getMatchHatTricks result = homeHatTricks ++ awayHatTricks
                           where homeHatTricks = map (expand (homeTeam result) result) . extractHatTricks $ homeGoals result
                                 awayHatTricks = map (expand (awayTeam result) result) . extractHatTricks $ awayGoals result
                                 expand team res (player, count) = (player, count, team, convertResult team res)

extractHatTricks :: [Goal] -> [(ByteString, Int)]
extractHatTricks goalsList = map (pair (scorer.head) length) byScorer
                             where byScorer = filter ((>= 3).length) . groupByScorer scorer $ filter ((/= "o").goalType) goalsList

-- | Hat-tricks are ordered first by the number of goals and then by date.
compareHatTrick :: (ByteString, Int, Team, TeamResult) -> (ByteString, Int, Team, TeamResult) -> Ordering
compareHatTrick (_, c1, _, r1) (_, c2, _, r2)
    | c1 == c2  = comparing day r1 r2
    | otherwise = compare c2 c1

-- | Divide the 90 minutes up into 10-minute intervals and return the number of goals scored and conceded in each interval.
goalsByInterval :: [TeamResult] -> ([Int], [Int])
goalsByInterval results = (goalsByInterval' $ concatMap goalsFor results, goalsByInterval' $ concatMap goalsAgainst results)

-- | Divide the 90 minutes up into 10-minute intervals and return the number of goals in each interval (ignore any extra-time goals).
goalsByInterval' :: [Goal] -> [Int]
goalsByInterval' gs = take 9 . Map.elems $ foldl' (\m g -> Map.alter alterTally (interval g) m) initialMap gs
                      where initialMap = Map.fromAscList . zip [0..8] $ repeat 0
                            interval g = (minute g - 1) `div` 10
                            alterTally = Just.(+1).fromMaybe 0

