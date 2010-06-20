{-# LANGUAGE DeriveDataTypeable #-}

-- | Core functionality for the Anorak system.
module Anorak.Results (aggregate,
                       awayWins,
                       biggestWins,
                       convertResult,
                       form,
                       Goal(..),
                       highestAggregates,
                       homeWins,
                       partitionResults,
                       prepareResults,
                       Result(..),
                       Results(..),
                       Team,
                       TeamResult(..)) where

import Anorak.Utils(equal, takeAtLeast)
import Data.ByteString.Char8(ByteString)
import qualified Data.ByteString.Char8 as BS(unpack)
import Data.List(groupBy, partition, sortBy)
import Data.Map(Map)
import qualified Data.Map as Map(empty, findWithDefault, insertWith, map, mapWithKey)
import Data.Ord(comparing)
import Data.Time.Calendar(Day(..))
import Data.Time.Format(formatTime)
import System.Locale(defaultTimeLocale)

-- | A team is represented simply by its name.
type Team = ByteString

-- | A match result consists of a date, two teams and the goals scored by each.
data Result = Result {date :: !Day,        -- ^ The day that the match was played.
                      homeTeam :: !Team,   -- ^ The team playing at home.
                      homeScore :: !Int,   -- ^ The number of goals scored by the home team.
                      awayTeam :: !Team,   -- ^ The visiting team.
                      awayScore :: !Int,   -- ^ The number of goals scored by the away team.
                      homeGoals :: [Goal], -- ^ An optional list of home goals.
                      awayGoals :: [Goal]} -- ^ An optional list of away goals.
instance Show Result where
    show result = formatTime defaultTimeLocale "%e %b %Y: " (date result) ++
                  BS.unpack (homeTeam result) ++ " " ++
                  show (homeScore result) ++ " - " ++
                  show (awayScore result) ++ " " ++
                  BS.unpack (awayTeam result)
instance Eq Result where
    (==) result1 result2 = date result1 == date result2
                           && homeTeam result1 == homeTeam result2
                           && awayTeam result1 == awayTeam result2
instance Ord Result where
    compare result1 result2
        | date result1 == date result2 = comparing homeTeam result1 result2
        | otherwise                    = comparing date result1 result2

-- | Returns the match aggregate (total number of goals).
aggregate :: Result -> Int
aggregate result = homeScore result + awayScore result

-- | A TeamResult is another way of organising information about the result of the match, relative to
--   a particular team.
data TeamResult = TeamResult {day :: !Day,
                              opposition :: !Team,
                              venue :: !Char,
                              scored :: !Int,
                              conceded :: !Int,
                              outcome :: !Char,
                              goals :: [Goal]}
instance Show TeamResult where
    show result = formatTime defaultTimeLocale "%e %b %Y: " (day result) ++
                  BS.unpack (opposition result) ++ "(" ++ [venue result] ++ ") " ++ [outcome result] ++ " " ++
                  show (scored result) ++ "-" ++ show (conceded result)

-- | The data about a goal consists of the name of the player who scored it and the minute of the match (1-90) in which it was scored.
data Goal = Goal {scorer :: !ByteString,
                  minute :: !Int,
                  goalType :: !ByteString}
    deriving (Eq, Show)

-- | Data structure for holding the different variants of the results data set.
data Results = Results {list :: ![Result],
                        byTeam :: !(Map Team [Result]),
                        homeOnly :: !(Map Team [Result]),
                        awayOnly :: !(Map Team [Result]),
                        byDate :: !(Map Day [Result]),
                        firstHalf :: Map Team [Result],
                        secondHalf :: Map Team [Result]}
                        
-- | Convert a flat list of results into a mapping from team to list of results that that team was involved in.
resultsByTeam :: [Result] -> Map ByteString Team -> Map Team [Result]
resultsByTeam [] _                                    = Map.empty
resultsByTeam (Result d ht hs at as hg ag:rs) aliases = addResultToMap (addResultToMap (resultsByTeam rs aliases) homeTeam aliasedHResult) awayTeam aliasedAResult
                                                        -- Map aliased team names so that all results for the same team are recorded against the current team name.
                                                        where hTeam = Map.findWithDefault ht ht aliases
                                                              aTeam = Map.findWithDefault at at aliases
                                                              aliasedHResult = Result d hTeam hs at as hg ag
                                                              aliasedAResult = Result d ht hs aTeam as hg ag

-- | Convert a flat list of results into a mapping from date to list of matches played on that date.
resultsByDate :: [Result] -> Map Day [Result]
resultsByDate []          = Map.empty
resultsByDate (result:rs) = addResultToMap (resultsByDate rs) date result

-- | Helper function for adding a result to a map that maps an aribtrary key type to a list of results.
addResultToMap :: Ord k => Map k [Result] -> (Result -> k) -> Result -> Map k [Result]
addResultToMap rMap keyFunction result = Map.insertWith (++) (keyFunction result) [result] rMap

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
    | homeScore result == awayScore result                                = 'D'
    | (homeTeam result == team && homeScore result > awayScore result)
      || (awayTeam result == team && awayScore result > homeScore result) = 'W'
    | otherwise                                                           = 'L'

-- | Converts a Result into a TeamResult for the specified team.
convertResult :: Team -> Result -> TeamResult
convertResult team result
    | team == homeTeam result = TeamResult (date result) (awayTeam result) 'H' (homeScore result) (awayScore result) (form team result) (homeGoals result)
    | otherwise               = TeamResult (date result) (homeTeam result) 'A' (awayScore result) (homeScore result) (form team result) (awayGoals result)

-- | Returns the margin of victory for a result (zero if it is a draw).
margin :: Result -> Int
margin result = abs $ homeScore result - awayScore result

homeWins :: [Result] -> [Result]
homeWins = filter (\r -> homeScore r > awayScore r)

awayWins :: [Result] -> [Result]
awayWins = filter (\r -> homeScore r < awayScore r)

biggestWins :: [Result] -> [Result]
biggestWins results = takeAtLeast 3 $ groupBy (equal margin) sortedResults
                      where sortedResults = sortBy (flip $ comparing margin) results

highestAggregates :: [Result] -> [Result]
highestAggregates results = takeAtLeast 3 $ groupBy (equal aggregate) sortedResults
                            where sortedResults = sortBy (flip $ comparing aggregate) results

-- | Covert a list of 90-minute results into hypothetical first-half and second-half results.
splitFirstAndSecondHalf :: [Result] -> ([Result], [Result])
splitFirstAndSecondHalf = unzip . map splitResult

-- | Convert a single 90-minute result into hypothetical first-half and second-half results.
splitResult :: Result -> (Result, Result)
splitResult (Result d ht _ at _ hg ag) = (firstHalfResult, secondHalfResult)
                                         where (fstHalfHGoals, laterHGoals) = partition ((<= 45).minute) hg
                                               (fstHalfAGoals, laterAGoals) = partition ((<= 45).minute) ag
                                               -- Filter out any extra-time goals.
                                               sndHalfHGoals = filter ((<= 90).minute) laterHGoals
                                               sndHalfAGoals = filter ((<= 90).minute) laterAGoals
                                               firstHalfResult = Result d ht (length fstHalfHGoals) at (length fstHalfAGoals) fstHalfHGoals fstHalfAGoals
                                               secondHalfResult = Result d ht (length sndHalfHGoals) at (length sndHalfAGoals) sndHalfHGoals sndHalfAGoals

-- | Take a list of results and return a set of useful transformations of that data.
prepareResults :: [Result] -> Map ByteString Team -> Results
prepareResults results aliases = Results results teamResults (Map.map fst homeAndAway) (Map.map snd homeAndAway) matchDays firstHalfByTeam secondHalfByTeam
                                 where teamResults = resultsByTeam results aliases
                                       matchDays = resultsByDate results
                                       (firstHalfResults, secondHalfResults) = splitFirstAndSecondHalf results
                                       firstHalfByTeam = resultsByTeam firstHalfResults aliases
                                       secondHalfByTeam = resultsByTeam secondHalfResults aliases
                                       homeAndAway = splitHomeAndAway teamResults

