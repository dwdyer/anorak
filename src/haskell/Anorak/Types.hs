-- | Core data types used by the Anorak system.
module Anorak.Types where

import Data.Time.Calendar(Day)
import Data.Time.Format(formatTime)
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
    (==) result1 result2 = (date result1) == (date result2)
                           && (homeTeam result1) == (homeTeam result2)
                           && (awayTeam result1) == (awayTeam result2)
instance Ord Result where
    compare result1 result2
        | (date result1) == (date result2) = compare (homeTeam result1) (homeTeam result2)
        | otherwise                        = compare (date result1) (date result2)

-- A LeagueRecord contains data about the league performance of a single team.  It
-- includes total number of wins, draws, defeats, goals scored and goals conceded.
data LeagueRecord = LeagueRecord {team :: Team,      -- ^ The team that this record relates to.
                                  won :: Int,        -- ^ The number of matches won by this team.
                                  drawn :: Int,      -- ^ The number of matches drawn by this team.
                                  lost :: Int,       -- ^ The number of matches lost by this team.
                                  for :: Int,        -- ^ The total number of goals scored by this team.
                                  against :: Int,    -- ^ The total number of goals conceded by this team.
                                  adjustment :: Int  -- ^ A points adjustment (can be positive or negative but is usually zero) to be applied to this team's total.
                                 }
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
        | (points record1) /= (points record2)     = compare (points record2) (points record1)
        | (goalDiff record1) /= (goalDiff record2) = compare (goalDiff record2) (goalDiff record1)
        | (for record1) /= (for record2)           = compare (for record2) (for record1)
        | (won record1) /= (won record2)           = compare (won record2) (won record1)
        | otherwise                                = EQ

-- | Calculates the total number of matches played (the sum or wins, draws and defeats).
played :: LeagueRecord -> Int
played record = (won record) + (drawn record) + (lost record)

-- | Calculates the total number of points (3 points for each win, 1 for each draw, +/- any adjustment).
points :: LeagueRecord -> Int
points record = (won record) * 3 + (drawn record) + (adjustment record)

-- | Calculates goal difference (total scored minus total conceded)
goalDiff :: LeagueRecord -> Int
goalDiff record = (for record) - (against record)

pointsPerGame :: LeagueRecord -> Double
pointsPerGame record = (fromIntegral $ points record) / (fromIntegral $ played record)

