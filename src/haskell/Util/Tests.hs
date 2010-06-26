-- | QuickCheck tests for the various utilities modules.
module Util.Tests (tests) where

import Test.QuickCheck.Batch(run)
import Util.List

-- | The list returned by takeAtLeast must respect the \"at least\" parameter, unless there are not enough elements.
prop_takeAtLeastLength :: Int -> [[Int]] -> Bool
prop_takeAtLeastLength n ls = length (takeAtLeast n ls) >= minSize
                              where minSize = min n (sum $ map length ls)

-- | If there are more than enough elements in the next group, we must still take the whole group of equivalents.
prop_takeAtLeastEquivalents :: [Int] -> Bool
prop_takeAtLeastEquivalents ls = length (takeAtLeast 3 [[1, 2], ls]) == 2 + length ls

tests = [run prop_takeAtLeastLength,
         run prop_takeAtLeastEquivalents]
