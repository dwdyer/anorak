{-# LANGUAGE DeriveDataTypeable #-}

-- | Core functionality for the Anorak system.
module Anorak.Results (aggregate, awayWins, biggestWins, convertResult, form, highestAggregates, homeWins, Result(..), resultsByDate, resultsByTeam, splitHomeAndAway, Team, TeamResult(..)) where

import Anorak.Utils(takeAtLeast)
import Data.Data(Data)
import Data.Map(Map)
import qualified Data.Map as Map(elems, empty, filterWithKey, findWithDefault, insertWith, map, mapWithKey)
import Data.Set(Set)
import qualified Data.Set as Set(member)
import Data.Time.Calendar(Day(..))
import Data.Time.Format(formatTime)
import Data.Typeable(Typeable)
import List(groupBy, partition, sort, sortBy)
import System.Locale(defaultTimeLocale)

-- | A team is represented simply by its name.
type Team = String

-- | A match result consists of a date, two teams and the goals scored by each.
data Result = Result {date :: Day,      -- ^ The day that the match was played.
                      homeTeam :: Team, -- ^ The team playing at home.
                      homeGoals :: Int, -- ^ The number of goals scored by the home team.
                      awayTeam :: Team, -- ^ The visiting team.
                      awayGoals :: Int  -- ^ The number of goals scored by the away team.
                     }
instance Show Result where
    show result = formatTime defaultTimeLocale "%e %b %Y: " (date result) ++
                  homeTeam result ++ " " ++
                  show (homeGoals result) ++ " - " ++
                  show (awayGoals result) ++ " " ++
                  awayTeam result
instance Eq Result where
    (==) result1 result2 = date result1 == date result2
                           && homeTeam result1 == homeTeam result2
                           && awayTeam result1 == awayTeam result2
instance Ord Result where
    compare result1 result2
        | date result1 == date result2 = compare (homeTeam result1) (homeTeam result2)
        | otherwise                    = compare (date result1) (date result2)

-- | Returns the match aggregate (total number of goals).
aggregate :: Result -> Int
aggregate result = homeGoals result + awayGoals result

-- | A TeamResult is another way of organising information about the result of the match, relative to
--   a particular team.
data TeamResult = TeamResult {day :: Day,
                              opposition :: Team,
                              venue :: Char,
                              scored :: Int,
                              conceded :: Int,
                              outcome :: Char}
instance Show TeamResult where
    show result = formatTime defaultTimeLocale "%e %b %Y: " (day result) ++
                  opposition result ++ "(" ++ [venue result] ++ ") " ++ [outcome result] ++ " " ++
                  show (scored result) ++ "-" ++ show (conceded result)

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
splitHomeAndAway = Map.mapWithKey partitionResults

-- | Splits a team's results into home results and away results.  The first item in the tuple is the home results, the
--   second is the away results.
partitionResults :: Team -> [Result] -> ([Result], [Result])
partitionResults team = partition (\x -> team == homeTeam x)

-- | Maps a single result for a particular team to a character ('W', 'D' or 'L').
form :: Team -> Result -> Char
form team result
    | homeGoals result == awayGoals result                                = 'D'
    | (homeTeam result == team && homeGoals result > awayGoals result)
      || (awayTeam result == team && awayGoals result > homeGoals result) = 'W'
    | otherwise                                                           = 'L'

-- | Converts a Result into a TeamResult for the specified team.
convertResult :: Team -> Result -> TeamResult
convertResult team result
    | team == homeTeam result = TeamResult (date result) (awayTeam result) 'H' (homeGoals result) (awayGoals result) (form team result)
    | otherwise               = TeamResult (date result) (homeTeam result) 'A' (awayGoals result) (homeGoals result) (form team result)

-- | Returns the margin of victory for a result (zero if it is a draw).
margin :: Result -> Int
margin result = abs $ homeGoals result - awayGoals result

homeWins :: [Result] -> [Result]
homeWins = filter (\r -> homeGoals r > awayGoals r)

awayWins :: [Result] -> [Result]
awayWins = filter (\r -> homeGoals r < awayGoals r)

biggestWins :: [Result] -> [Result]
biggestWins results = takeAtLeast 3 $ groupBy (\r1 r2 -> margin r1 == margin r2) sortedResults
                      where sortedResults = sortBy (\r1 r2 -> compare (margin r2) (margin r1)) results

highestAggregates :: [Result] -> [Result]
highestAggregates results = takeAtLeast 3 $ groupBy (\r1 r2 -> aggregate r1 == aggregate r2) sortedResults
                            where sortedResults = sortBy (\r1 r2 -> compare (aggregate r2) (aggregate r1)) results

