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
                      awayGoals :: [Goal]  -- ^ An optional list of away goals.
                     }
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
                           && homeScore result1 == homeScore result2
                           && awayScore result1 == awayScore result2
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
                              goalsFor :: [Goal],
                              goalsAgainst :: [Goal]}
instance Show TeamResult where
    show result = formatTime defaultTimeLocale "%e %b %Y: " (day result) ++
                  BS.unpack (opposition result) ++ "(" ++ [venue result] ++ ") " ++ [outcome result] ++ " " ++
                  show (scored result) ++ "-" ++ show (conceded result)

-- | The data about a goal consists of the name of the player who scored it and the minute of the match (1-90) in which it was scored.
data Goal = Goal {scorer :: !ByteString,  -- ^ The name of the player that scored the goal.
                  minute :: !Int,         -- ^ The minute (1-90) in which the goal was scored.  First-half injury time is always 45, second-half added time always 90.
                  goalType :: !ByteString -- ^ \"p\" for penalty, \"o\" for own goal, empty string for all others.
                 }
    deriving (Eq, Show)

-- | Data structure for holding the different variants of the results data set.
data Results = Results {list :: ![Result],                -- ^ A list of all results for all teams for the season, ordered by date.
                        byTeam :: !(Map Team [Result]),   -- ^ Map from team name to list of results that team was involved in.
                        homeOnly :: !(Map Team [Result]), -- ^ Home results by team.
                        awayOnly :: !(Map Team [Result]), -- ^ Away results by team.
                        byDate :: !(Map Day [Result]),    -- ^ Results by date, each date is mapped to a list of matches on that day.
                        firstHalf :: Map Team [Result],   -- ^ Half-time scores by team.
                        secondHalf :: Map Team [Result]   -- ^ Second-half scores by team.
                       }
                        
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
    | team == homeTeam result = TeamResult (date result) (awayTeam result) 'H' (homeScore result) (awayScore result) (form team result) (homeGoals result) (awayGoals result)
    | otherwise               = TeamResult (date result) (homeTeam result) 'A' (awayScore result) (homeScore result) (form team result) (awayGoals result) (homeGoals result)

-- | Returns the margin of victory for a result (zero if it is a draw).
margin :: Result -> Int
margin result = abs $ homeScore result - awayScore result

-- | Filter the list of results and return only matches in which the home team won (omit draws and away wins).
homeWins :: [Result] -> [Result]
homeWins = filter (\r -> homeScore r > awayScore r)

-- | Filter the list of results and return only matches in which the away team won (omit draws and home wins).
awayWins :: [Result] -> [Result]
awayWins = filter (\r -> homeScore r < awayScore r)

-- | Return the results with the biggest margins of victory.  Returns at least 3 results (unless
--   there are less than 3 in total).  Any results that are equivalent to the third result are also
--   returned.  So, for example, if the biggest margin is 4 goals and there are 5 matches with
--   a 4-goal margin, all 5 will be returned.
biggestWins :: [Result] -> [Result]
biggestWins results = takeAtLeast 3 $ groupBy (equal margin) sortedResults
                      where sortedResults = sortBy (flip $ comparing margin) results

-- | Return the results with the highest total number of goals.  Returns at least 3 results (unless
--   there are less than 3 in total).  Any results that are equivalent to the third result are also
--   returned.  So, for example, if the highest aggregate is 10 goals and there are 5 matches with
--   10 goals, all 5 will be returned.
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

