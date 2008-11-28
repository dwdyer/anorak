-- | HTML publishing module for the Anorak system.
module Anorak.Publisher where

import Anorak.Core
import Anorak.Types
import Anorak.RLTParser
import Data.Map(Map)
import Data.Time.Calendar(Day)
import qualified Data.Map as Map(empty, fromAscList, map, toList)
import List(isPrefixOf, isSuffixOf)
import Monad(filterM)
import System(getArgs)
import System.Directory(createDirectoryIfMissing, copyFile, doesFileExist, getDirectoryContents)
import System.FilePath(combine, replaceDirectory, takeFileName)
import Text.ParserCombinators.Parsec(ParseError)
import Text.StringTemplate(directoryGroup, getStringTemplate, setAttribute, setManyAttrib, STGroup, StringTemplate, stShowsToSE, toString)
import Text.StringTemplate.Classes(ToSElem(toSElem), SElem(SM))

instance ToSElem LeagueRecord where
    toSElem record = SM $ Map.fromAscList [("against", toSElem $ against record),
                                           ("average", stShowsToSE $ pointsPerGame record),
                                           ("drawn", toSElem $ drawn record),
                                           ("for", toSElem $ for record),
                                           ("goalDiff", toSElem $ goalDiff record),
                                           ("lost", toSElem $ lost record),
                                           ("negativeGD", toSElem $ goalDiff record < 0), -- Goal difference could be neither +ve or -ve (i.e. zero).
                                           ("played", toSElem $ played record),
                                           ("points", toSElem $ points record),
                                           ("positiveGD", toSElem $ goalDiff record > 0), -- Goal difference could be neither +ve or -ve (i.e. zero).
                                           ("team", toSElem $ team record),
                                           ("won", toSElem $ won record)]

instance ToSElem Result where
    toSElem result = SM $ Map.fromAscList [("awayGoals", toSElem $ awayGoals result),
                                           ("awayTeam", toSElem $ awayTeam result),
                                           ("date", toSElem $ date result),
                                           ("homeGoals", toSElem $ homeGoals result),
                                           ("homeTeam", toSElem $ homeTeam result)]

-- | Copies all non-template files from the source directory to the target directory.  Used for making sure that CSS
--   files and images (if any) are deployed with the generated HTML.  If the target directory does not exist it is
--   created.
copyResources :: FilePath -> FilePath -> IO ()
copyResources from to = do files <- getDirectoryContents from
                           let absoluteFiles = map (combine from) files -- Convert file names into absolute paths.
                           resources <- filterM (isResourceFile) absoluteFiles
                           print resources
                           createDirectoryIfMissing True to
                           mapM_ (copyToDirectory to) resources

-- | Predicate for filtering.  Accepts files that are not templates, not directories and not hidden files.
isResourceFile :: FilePath -> IO Bool
isResourceFile path = do ordinaryFile <- doesFileExist path
                         return (ordinaryFile
                                 && (not $ isSuffixOf ".st" path) -- File name does not have the template extension.
                                 && (not $ isPrefixOf "." $ takeFileName path)) -- File is not a hidden file.

-- | Copies an individual file to a new directory, retaining the original file name.
copyToDirectory :: FilePath -> FilePath -> IO()
copyToDirectory dir file = copyFile file (replaceDirectory file dir)

-- | Generates an output file by applying a template with one or more attributes set.
applyTemplate :: ToSElem a => STGroup String -> FilePath -> FilePath -> [(String, a)] -> IO ()
applyTemplate group templateName dir attributes = case getStringTemplate templateName group of
                                                      Nothing       -> print $ "Could not find template for " ++ templateName
                                                      Just template -> writeFile (combine dir templateName) html
                                                                       where html = toString $ setManyAttrib attributes template

-- | Generates home, away and overall HTML league tables.
generateLeagueTables :: STGroup String -> FilePath -> Map Team [Result] -> Map Team Int -> IO ()
generateLeagueTables group dir results adjustments = do applyTemplate group "overalltable.html" dir [("table", leagueTable results adjustments)]
                                                        let splitResults = splitHomeAndAway results
                                                        applyTemplate group "hometable.html" dir [("table", leagueTable (Map.map fst splitResults) Map.empty)]
                                                        applyTemplate group "awaytable.html" dir [("table", leagueTable (Map.map snd splitResults) Map.empty)]

generateFormTables :: STGroup String -> FilePath -> Map Team [Result] -> IO ()
generateFormTables group dir results = do applyTemplate group "overallformtable.html" dir [("table", formTable results 6)]
                                          let splitResults = splitHomeAndAway results
                                          applyTemplate group "homeformtable.html" dir [("table", formTable (Map.map fst splitResults) 4)]
                                          applyTemplate group "awayformtable.html" dir [("table", formTable (Map.map snd splitResults) 4)]

generateResultsList :: STGroup String -> FilePath -> Map Day [Result] -> IO ()
generateResultsList group dir results = applyTemplate group "results.html" dir [("results", Map.toList results)]

-- | Expects three arguments - the path to the RLT data file, the path to the templates directory and the path to the
--   output directory.
main :: IO ()
main = do dataFile:templateDir:outputDir:_ <- getArgs
          (teams, results, adjustments) <- parseRLTFile dataFile
          group <- directoryGroup templateDir :: IO (STGroup String)
          copyResources templateDir outputDir
          let teamResults = resultsByTeam results
          generateLeagueTables group outputDir teamResults adjustments
          generateFormTables group outputDir teamResults
          generateResultsList group outputDir (resultsByDate results)

