-- | Utility functions for working with lists.
module Util.List (equal,
                  keep,
                  takeAtLeast) where

-- | Retains the last n elements in a list.
keep :: Int -> [a] -> [a]
keep n x = drop (length x - n) x

-- | Return the first group from a list of groups, but if it's too short append the next group(s) until it's long enough.
takeAtLeast :: Int -> [[a]] -> [a]
takeAtLeast _ []               = [] -- If we run out of groups, we can't take any more even if we haven't reached the minimum yet.
takeAtLeast 0 _                = [] -- If we've reached the minimum we're done.
takeAtLeast count (group:rest) = group ++ takeAtLeast (max 0 (count - length group)) rest

-- | Analogous to comparing in Data.Ord.  Allows for indirect equality checks.  Useful for use with groupBy.
equal :: (Eq a) => (b -> a) -> b -> b -> Bool
equal f x y = f x == f y
