-- | Helper functions for working with directories and files.
module Util.File (copyMatchingFiles,
                  isNewer,
                  makeAbsolute) where

import Control.Exception(try)
import Control.Monad(filterM)
import Data.List(isPrefixOf)
import System.Directory(copyFile, createDirectoryIfMissing, doesDirectoryExist, doesFileExist, getDirectoryContents, getModificationTime)
import System.FilePath((</>), isRelative, replaceDirectory)
import System.IO.Error(isDoesNotExistError)

-- | If the first path is relative, use the second path (which must be a directory) to make it absolute.
makeAbsolute :: FilePath -> FilePath -> FilePath
makeAbsolute path base
    | isRelative path = base </> path
    | otherwise       = path

-- | Copies an individual file to a new directory, retaining the original file name.
copyToDirectory :: FilePath -> FilePath -> IO ()
copyToDirectory dir file = copyFile file (replaceDirectory file dir)

-- | Returns true if the first file is newer than the second.  If the destination file does not exist, returns true.
--   If the source file does not exist, throws an IOError.
isNewer :: FilePath -> FilePath -> IO Bool
isNewer source destination = do sourceTime <- getModificationTime source
                                result <- try $ getModificationTime destination
                                case result of
                                    Left e          -> if isDoesNotExistError e then return True else ioError e
                                    Right destTime  -> return $ sourceTime > destTime

-- | Returns a list of files (excluding hidden files) in the specified directory.  The returned paths
--   are fully-qualified.
listFiles :: FilePath -> IO [FilePath]
listFiles dir = do contents <- getDirectoryContents dir
                   let visible = filter (not . isPrefixOf ".") contents -- Exclude hidden files.
                   return $ map (dir </>) visible -- Use qualified paths.

-- | Copies all non-template files from the source directory to the target directory.  Used for making sure that CSS
--   files and images (if any) are deployed with the generated HTML.  If the target directory does not exist it is
--   created.
copyMatchingFiles :: (FilePath -> Bool) -> FilePath -> FilePath -> IO ()
copyMatchingFiles match from to = do files <- listFiles from
                                     dirs <- filterM doesDirectoryExist files
                                     nonDirs <- filterM doesFileExist files
                                     let resources = filter match nonDirs
                                     createDirectoryIfMissing True to
                                     mapM_ (copyToDirectory to) resources
                                     mapM_ (\x -> copyMatchingFiles match x $ replaceDirectory x to) dirs
