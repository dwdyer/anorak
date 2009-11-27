-- | Feature extraction for results data.  These features and their associated match outcomes can be used to
--   train classifiers that attempt to predict results.
module Anorak.FeatureExtractor (generateFeatures, MatchFeatures(..), Outcome, TeamFeatures(..)) where

import Anorak.Results(Result(..), Team)
import Anorak.RLTParser(LeagueData(..), parseRLTFile)
import Anorak.Utils(rate)
import Data.Map(Map, (!))
import qualified Data.Map as Map(adjustWithKey, empty, fromDistinctAscList)
import qualified Data.Set as Set(toAscList)

-- | There are only three possible outcomes for a match.
data Outcome = HomeWin | Draw | AwayWin deriving (Show)

data TeamFeatures = TeamFeatures {matches :: Int,
                                  wins :: Int,
                                  draws :: Int,
                                  goalsFor :: Int,
                                  goalsAgainst :: Int}
-- | Team features are written out as tab-separted data with 4 columns: win rate, draw rate, score rate and concede rate.
instance Show TeamFeatures where
    show (TeamFeatures p w d f a) = (show $ rate w p) ++ "\t" ++ (show $ rate d p) ++ "\t" ++ (show $ rate f p) ++ "\t" ++ (show $ rate a p)

data MatchFeatures = MatchFeatures TeamFeatures TeamFeatures Outcome
-- | Match features are just the home team and away team features combined, giving 8 tab-separated columns.
instance Show MatchFeatures where
    show (MatchFeatures h a o) = show h ++ "\t" ++ show a ++ "\t" ++ show o

generateFeatures :: FilePath -> IO ()
generateFeatures dataFile = do LeagueData teams results _ _ <- parseRLTFile dataFile
                               let features = extractFeatures (Set.toAscList teams) results
                               mapM_ print features

-- | Given a list of results, return a list of features and associated match outcomes.
extractFeatures :: [Team] -> [Result] -> [MatchFeatures]
extractFeatures teams results = extractFeatures' results emptyRecords 
                                where emptyRecords = (Map.fromDistinctAscList (map (\t -> (t, TeamFeatures 0 0 0 0 0)) teams))

extractFeatures' :: [Result] -> Map Team TeamFeatures -> [MatchFeatures]
extractFeatures' [] teamRecords = []
extractFeatures' (r:rs) teamRecords = (getResultFeatures r teamRecords):extractFeatures' rs (updateRecords r teamRecords)

-- | After processing a result, we add it to the form records of the teams involved so that it can be used as
--   form data for subsequent matches.
updateRecords :: Result -> Map Team TeamFeatures -> Map Team TeamFeatures
updateRecords result teamRecords = Map.adjustWithKey adjust (awayTeam result) (Map.adjustWithKey adjust (homeTeam result) teamRecords)
                                   where adjust = \k v -> addResultToFeatures k v result

-- | Update a team's form record with the result of the specified match.
addResultToFeatures :: Team -> TeamFeatures -> Result -> TeamFeatures
addResultToFeatures team features result
    | team == homeTeam result = addScoreToFeatures features (homeGoals result) (awayGoals result)
    | team == awayTeam result = addScoreToFeatures features (awayGoals result) (homeGoals result)
    | otherwise               = features

addScoreToFeatures :: TeamFeatures -> Int -> Int -> TeamFeatures
addScoreToFeatures (TeamFeatures p w d f a) scored conceded
    | scored > conceded  = TeamFeatures (p + 1) (w + 1) d (f + scored) (a + conceded)
    | scored == conceded = TeamFeatures (p + 1) w (d + 1) (f + scored) (a + conceded)
    | otherwise          = TeamFeatures (p + 1) w d (f + scored) (a + conceded)

-- | Get the form data and match outcome for a given result.
getResultFeatures :: Result -> Map Team TeamFeatures -> MatchFeatures
getResultFeatures result teamRecords = MatchFeatures homeRecord awayRecord (getOutcome result)
                                       where homeRecord = teamRecords ! homeTeam result
                                             awayRecord = teamRecords ! awayTeam result

-- | Maps a result to one of three possible outcome types (home win, away win or draw).
getOutcome :: Result -> Outcome
getOutcome result
    | homeGoals result > awayGoals result = HomeWin
    | awayGoals result > homeGoals result = AwayWin
    | otherwise                           = Draw
