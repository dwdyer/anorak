-- | Feature extraction for results data.  These features and their associated match outcomes can be used to
--   train classifiers that attempt to predict results.
module Anorak.FeatureExtractor (generateFeatures, MatchFeatures(..), Outcome, TeamFeatures(..)) where

import Anorak.Results(Result(..), Team)
import Anorak.RLTParser(LeagueData(..), parseRLTFile)
import Anorak.Utils(rate)
import Data.Map(Map, (!))
import qualified Data.Map as Map(adjustWithKey, empty, fromDistinctAscList, insert, lookup, notMember)
import qualified Data.Set as Set(toAscList)

-- | There are only three possible outcomes for a match.
data Outcome = HomeWin | Draw | AwayWin deriving (Show)

data TeamFeatures = TeamFeatures {winRate :: Double,
                                  drawRate :: Double,
                                  scoreRate :: Double,
                                  concedeRate :: Double}
-- | Team features are written out as tab-separted data with 4 columns: win rate, draw rate, score rate and concede rate.
instance Show TeamFeatures where
    show (TeamFeatures w d f a) = show w ++ "\t" ++ show d ++ "\t" ++ show f ++ "\t" ++ show a

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
extractFeatures teams results = extractFeatures' results Map.empty

extractFeatures' :: [Result] -> Map Team TeamFeatures -> [MatchFeatures]
extractFeatures' [] teamRecords = []
extractFeatures' (r@(Result _ hteam _ ateam _):rs) teamRecords
    -- If we don't have features for the teams involved we initialise them from this result, but we can't generate
    -- match features from nothing so we skip the result and continue with subsequent results.
    | Map.notMember hteam teamRecords || Map.notMember ateam teamRecords = otherFeatures
    | otherwise                                                          = getResultFeatures r teamRecords : otherFeatures
    where otherFeatures = extractFeatures' rs (updateRecords r teamRecords)

-- | After processing a result, we add it to the form records of the teams involved so that it can be used as
--   form data for subsequent matches.
updateRecords :: Result -> Map Team TeamFeatures -> Map Team TeamFeatures
updateRecords result@(Result _ hteam hgoals ateam agoals) teamRecords = Map.insert ateam awayFeatures (Map.insert hteam homeFeatures teamRecords)
                                                                        where homeFeatures = case Map.lookup hteam teamRecords of
                                                                                                 Nothing -> initialFeatures hgoals agoals
                                                                                                 Just f  -> addResultToFeatures hteam f result
                                                                              awayFeatures = case Map.lookup ateam teamRecords of
                                                                                                 Nothing -> initialFeatures agoals hgoals
                                                                                                 Just f  -> addResultToFeatures ateam f result
                                                                        
-- | Create the intial team features from a team's first result.
initialFeatures :: Int -> Int -> TeamFeatures
initialFeatures for against
    | for > against = TeamFeatures 1 0 scored conceded
    | for < against = TeamFeatures 0 0 scored conceded
    | otherwise     = TeamFeatures 0 1 scored conceded
    where scored = fromIntegral for
          conceded = fromIntegral against

-- | Update a team's form record with the result of the specified match.
addResultToFeatures :: Team -> TeamFeatures -> Result -> TeamFeatures
addResultToFeatures team features (Result _ hteam hgoals ateam agoals)
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
getResultFeatures :: Result -> Map Team TeamFeatures -> MatchFeatures
getResultFeatures result teamRecords = MatchFeatures homeRecord awayRecord (getOutcome result)
                                       where homeRecord = teamRecords ! homeTeam result
                                             awayRecord = teamRecords ! awayTeam result

-- | Maps a result to one of three possible outcome types (home win, away win or draw).
getOutcome :: Result -> Outcome
getOutcome (Result _ _ hgoals _ agoals)
    | hgoals > agoals = HomeWin
    | agoals > hgoals = AwayWin
    | otherwise       = Draw
