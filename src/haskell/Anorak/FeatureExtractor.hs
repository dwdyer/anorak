-- | Feature extraction for results data.  These features and their associated match outcomes can be used to
--   train classifiers that attempt to predict results.
module Anorak.FeatureExtractor (generateFeatures,
                                MatchFeatures(..),
                                Outcome,
                                TeamFeatures(..)) where

import Anorak.Results(Result(..), Team)
import Anorak.RLTParser(LeagueData(..), parseRLTFile)
import Data.Map(Map, (!))
import qualified Data.Map as Map(empty, insert, lookup, member)

-- | There are only three possible outcomes for a match.
data Outcome = HomeWin | Draw | AwayWin deriving (Show)

-- | Team features consist of four values, the weighted win ratio, weighted draw ratio then weighted score and concede ratios.
--   Values take into account all previous matches but each time a new result is added, the influence of previous results is
--   discounted.  The most recent result accounts for 25% of the value, second most recent result is 0.75 * 25% or 18.75%, and
--   so on.
data TeamFeatures = TeamFeatures Double Double Double Double
-- | Team features are written out as tab-separted data with 4 columns: win rate, draw rate, score rate and concede rate.
instance Show TeamFeatures where
    show (TeamFeatures w d f a) = show w ++ "\t" ++ show d ++ "\t" ++ show f ++ "\t" ++ show a

data MatchFeatures = MatchFeatures (TeamFeatures, TeamFeatures) (TeamFeatures, TeamFeatures) Outcome
-- | Match features are just the home team and away team features combined, giving 16 tab-separated columns.
instance Show MatchFeatures where
    show (MatchFeatures (h, hh) (a, aa) o) = show h ++ "\t" ++ show hh ++ "\t" ++ show a ++ "\t" ++ show aa ++ "\t" ++ show o

generateFeatures :: FilePath -> IO ()
generateFeatures dataFile = do LeagueData _ res _ _ _ _ _ <- parseRLTFile dataFile
                               let features = extractFeatures res Map.empty Map.empty Map.empty
                               mapM_ print features

-- | Given a list of results, return a list of features and associated match outcomes.
extractFeatures :: [Result] -> Map Team TeamFeatures -> Map Team TeamFeatures -> Map Team TeamFeatures -> [MatchFeatures]
extractFeatures [] _ _ _ = []
extractFeatures (r@(Result _ hteam _ ateam _ _ _):rs) overall home away
    -- If we don't have features for the teams involved we initialise them from this result, but we can't generate
    -- match features from nothing so we skip the result and continue with subsequent results.
    | not $ haveFeatures hteam ateam overall home away = otherFeatures
    | otherwise                                        = getResultFeatures r overall home away : otherFeatures
    where otherFeatures = extractFeatures rs (updateRecords ateam r $ updateRecords hteam r overall) (updateRecords hteam r home) (updateRecords ateam r away)

haveFeatures :: Team -> Team -> Map Team TeamFeatures -> Map Team TeamFeatures -> Map Team TeamFeatures -> Bool
haveFeatures hteam ateam overall home away = Map.member hteam overall && Map.member ateam overall && Map.member hteam home && Map.member ateam away

-- | After processing a result, we add it to the form records of the teams involved so that it can be used as
--   form data for subsequent matches.
updateRecords :: Team -> Result -> Map Team TeamFeatures -> Map Team TeamFeatures
updateRecords team result teamRecords = Map.insert team features teamRecords
                                        where features = case Map.lookup team teamRecords of
                                                             Nothing -> initialFeatures team result
                                                             Just f  -> addResultToFeatures team f result
                                                                        
-- | Create the intial team features from a team's first result.
initialFeatures :: Team -> Result -> TeamFeatures
initialFeatures team (Result _ ht hg _ ag _ _)
    | hg == ag              = TeamFeatures 0 1 (fromIntegral hg) (fromIntegral ag)
    | team == ht && hg > ag = TeamFeatures 1 0 (fromIntegral hg) (fromIntegral ag)
    | team == ht && hg < ag = TeamFeatures 0 0 (fromIntegral hg) (fromIntegral ag)
    | ag > hg               = TeamFeatures 1 0 (fromIntegral ag) (fromIntegral hg)
    | otherwise             = TeamFeatures 0 0 (fromIntegral ag) (fromIntegral hg)

-- | Update a team's form record with the result of the specified match.
addResultToFeatures :: Team -> TeamFeatures -> Result -> TeamFeatures
addResultToFeatures team features (Result _ hteam hgoals ateam agoals _ _)
    | team == hteam = addScoreToFeatures features hgoals agoals
    | team == ateam = addScoreToFeatures features agoals hgoals
    | otherwise     = features

addScoreToFeatures :: TeamFeatures -> Int -> Int -> TeamFeatures
addScoreToFeatures (TeamFeatures w d f a) scored conceded
    | scored > conceded  = TeamFeatures (w * 0.75 + 0.25) (d * 0.75) forRate againstRate
    | scored == conceded = TeamFeatures (w * 0.75) (d * 0.75 + 0.25) forRate againstRate
    | otherwise          = TeamFeatures (w * 0.75) (d * 0.75) forRate againstRate
    where forRate = f * 0.75 + fromIntegral scored * 0.25
          againstRate = a * 0.75 + fromIntegral conceded * 0.25

-- | Get the form data and match outcome for a given result.
getResultFeatures :: Result -> Map Team TeamFeatures -> Map Team TeamFeatures -> Map Team TeamFeatures -> MatchFeatures
getResultFeatures result overallRecords homeRecords awayRecords = MatchFeatures homeRecord awayRecord (getOutcome result)
                                                                  where homeRecord = (overallRecords ! homeTeam result, homeRecords ! homeTeam result)
                                                                        awayRecord = (overallRecords ! awayTeam result, awayRecords ! awayTeam result)

-- | Maps a result to one of three possible outcome types (home win, away win or draw).
getOutcome :: Result -> Outcome
getOutcome (Result _ _ hgoals _ agoals _ _)
    | hgoals > agoals = HomeWin
    | agoals > hgoals = AwayWin
    | otherwise       = Draw
