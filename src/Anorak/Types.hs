-- | Core data types used by the Anorak system.
module Anorak.Types (Team, Result(Result), Adjustment(Adjustment)) where

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

-- | An adjustment adds or subtracts league points from a team.  It is typically used to deduct
--   points from a team that has infringed the competition rules, although it may also be used
--   to add points to a team's total.
data Adjustment = Adjustment {team :: Team,
                              amount :: Int}
instance Show Adjustment where
    show adjustment = team adjustment ++ " " ++ show (amount adjustment)

