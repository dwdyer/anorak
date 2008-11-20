-- | Core data types used by the Anorak system.
module Anorak.Types where

import Data.Time.Calendar(Day)
import Data.Time.Format(formatTime)
import System.Locale(defaultTimeLocale)

-- | A team is represented simply by its name.
type Team = String

-- | A match result consists of two teams and the goals scored by each.
data Result = Result {date :: Day,
                      homeTeam :: Team,
                      homeGoals :: Int,
                      awayTeam :: Team,
                      awayGoals :: Int}
instance Show Result where
    show result = formatTime defaultTimeLocale "%e %b %Y: " (date result) ++
                  homeTeam result ++ " " ++
                  show (homeGoals result) ++ " - " ++
                  show (awayGoals result) ++ " " ++
                  awayTeam result
instance Eq Result where
    (==) result1 result2 = (date result1) == (date result2)
                           && (homeTeam result1) == (homeTeam result2)
                           && (awayTeam result1) == (awayTeam result2)
instance Ord Result where
    compare result1 result2
        | (date result1) == (date result2) = compare (homeTeam result1) (homeTeam result2)
        | otherwise                        = compare (date result1) (date result2)

-- A LeagueRecord contains data about the league performance of a single team.  It
-- includes total number of wins, draws, defeats, goals scored and goals conceded.
data LeagueRecord = LeagueRecord {team :: Team,
                                  won :: Int,
                                  drawn :: Int,
                                  lost :: Int,
                                  for :: Int,
                                  against :: Int}
-- A LeagueRecord can be rendered as a String containing both member fields and
-- derived fields.
instance Show LeagueRecord where
    show record = (team record) ++
                  " P" ++ show (played record) ++
                  " W" ++ show (won record) ++
                  " D" ++ show (drawn record) ++
                  " L" ++ show (lost record) ++
                  " F" ++ show (for record) ++
                  " A" ++ show (against record) ++
                  " GD" ++ show (goalDiff record) ++
                  " Pts" ++ show (points record)
-- A LeagueRecord can be compared with other records to provide an ordering for a
-- list of records.
instance Eq LeagueRecord where
    (==) record1 record2 = (points record1) == (points record2)
                           && (goalDiff record1) == (goalDiff record2)
                           && (for record1) == (for record2)
                           && (won record1) == (won record2)
instance Ord LeagueRecord where
    compare record1 record2
        | (points record1) /= (points record2)     = compare (points record1) (points record2)
        | (goalDiff record1) /= (goalDiff record2) = compare (goalDiff record1) (goalDiff record2)
        | (for record1) /= (for record2)           = compare (for record1) (for record2)
        | (won record1) /= (won record2)           = compare (won record1) (won record1)
        | otherwise                                = EQ

played :: LeagueRecord -> Int
played record = (won record) + (drawn record) + (lost record)

points :: LeagueRecord -> Int
points record = (won record) * 3 + (drawn record)

goalDiff :: LeagueRecord -> Int
goalDiff record = (for record) - (against record)

