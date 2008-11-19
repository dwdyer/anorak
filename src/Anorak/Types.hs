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

data Adjustment = Adjustment {team :: Team,
                              amount :: Int}

