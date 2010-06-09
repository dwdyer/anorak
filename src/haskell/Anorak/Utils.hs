-- | Utility functions that are not specific to Anorak.
module Anorak.Utils (copyToDirectory, equal, fst3, keep, isNewer, makeAbsolute, percentage, snd3, takeAtLeast, trd3) where

import System.Directory(copyFile, getModificationTime)
import System.FilePath(combine, isRelative, replaceDirectory)
import System.IO.Error(isDoesNotExistError, try)

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

-- | Returns true if the first file is newer than the second.  If the destination file does not exist, returns true.
--   If the source file does not exist, throws an IOError.
isNewer :: FilePath -> FilePath -> IO Bool
isNewer source destination = do sourceTime <- getModificationTime source
                                result <- try $ getModificationTime destination
                                case result of
                                    Left e          -> if isDoesNotExistError e then return True else ioError e
                                    Right destTime  -> return $ sourceTime > destTime

-- | Look-up the first item in a 3-tuple.
fst3 :: (a, b, c) -> a
fst3 (a, _, _) = a

-- | Look-up the second item in a 3-tuple.
snd3 :: (a, b, c) -> b
snd3 (_, b, _) = b

-- | Look-up the third item in a 3-tuple.
trd3 :: (a, b, c) -> c
trd3 (_, _, c) = c

-- | Analogous to comparing in Data.Ord.  Allows for indirect equality checks.  Useful for use with groupBy.
equal :: (Eq a) => (b -> a) -> b -> b -> Bool
equal f x y = f x == f y
