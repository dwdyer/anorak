{-# LANGUAGE OverloadedStrings #-}

-- | QuickCheck tests for the various Anorak modules.
module Anorak.Tests (tests) where

import Anorak.Results
import Data.List((\\), intersect)
import Data.Time.Calendar(fromGregorian)
import Test.QuickCheck(Arbitrary(..), choose, test, variant)
import Test.QuickCheck.Batch(run)

instance Arbitrary Result where
    arbitrary = do hScore <- choose(0, 10)
                   aScore <- choose(0, 10)
                   day <- choose (1, 31)
                   month <- choose (1, 12)
                   return $ Result (fromGregorian 2010 day month) "HomeTeam" hScore "AwayTeam" aScore [] []
    coarbitrary = undefined

-- | Home wins extracted from a list of results should only include results from the input list and the number of results should
--   be less than or equal to the total number of results.
prop_homeWinsConsistent :: [Result] -> Bool
prop_homeWinsConsistent results = length hWins <= length results && null (hWins \\ results)
                                  where hWins = homeWins results

-- | Away wins extracted from a list of results should only include results from the input list and the number of results should
--   be less than or equal to the total number of results.
prop_awayWinsConsistent :: [Result] -> Bool
prop_awayWinsConsistent results = length aWins <= length results && null (aWins \\ results)
                                  where aWins = awayWins results

-- | Each result should be classifiable as a home win, an away win or a draw with no overlap.
prop_nonWinsAreDraws :: [Result] -> Bool
prop_nonWinsAreDraws results = length draws + length hWins + length aWins == length results && null (draws `intersect` hWins) && null (draws `intersect` aWins)
                               where hWins = homeWins results
                                     aWins = awayWins results
                                     draws = filter (\r -> homeScore r == awayScore r)  results

tests = [run prop_homeWinsConsistent,
         run prop_awayWinsConsistent,
         run prop_nonWinsAreDraws]
