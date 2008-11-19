-- | Core data types used by the Anorak system.
module Anorak.Types (Team, Result(Result)) where

-- | A team is represented simply by its name.
type Team = String

-- | A match result consists of two teams and the goals scored by each.
data Result = Result {homeTeam :: Team,
                      homeGoals :: Int,
                      awayTeam :: Team,
                      awayGoals :: Int}
instance Show Result where
    show result = homeTeam result ++ " " ++
                  show (homeGoals result) ++ " - " ++
                  show (awayGoals result) ++ " " ++
                  awayTeam result

