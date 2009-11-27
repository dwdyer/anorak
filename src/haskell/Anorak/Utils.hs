-- | Utility functions that are not specific to Anorak.
module Anorak.Utils (copyToDirectory, keep, makeAbsolute, percentage, rate, takeAtLeast) where

import System.Directory(copyFile)
import System.FilePath(combine, isRelative, replaceDirectory)

-- | Retains the last n elements in a list.
keep :: Int -> [a] -> [a]
keep n x = drop (length x - n) x

-- | Return the first group from a list of groups, but if it's too short append the next group(s) until it's long enough.
takeAtLeast :: Int -> [[a]] -> [a]
takeAtLeast _ []               = [] -- If we run out of groups, we can't take any more even if we haven't reached the minimum yet.
takeAtLeast 0 _                = [] -- If we've reached the minimum we're done.
takeAtLeast count (group:rest) = group ++ takeAtLeast (max 0 (count - length group)) rest

percentage :: Int -> Int -> Float
percentage num denom = fromIntegral (num * 100) / fromIntegral denom

-- | If the first path is relative, use the second path (which must be a directory) to make it absolute.
makeAbsolute :: FilePath -> FilePath -> FilePath
makeAbsolute path base
    | isRelative path = combine base path
    | otherwise       = path

-- | Copies an individual file to a new directory, retaining the original file name.
copyToDirectory :: FilePath -> FilePath -> IO()
copyToDirectory dir file = copyFile file (replaceDirectory file dir)

-- | Calculate a floating point rate from the given numerator and denominator.
rate :: Int -> Int -> Double
rate n d = fromIntegral n / fromIntegral d
