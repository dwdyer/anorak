{-# LANGUAGE ExistentialQuantification #-}

-- | HTML publishing module for the Anorak system.
module Anorak.Publisher where

import Anorak.Core
import Anorak.Types
import Anorak.RLTParser
import Data.Map(Map)
import Data.Set(Set)
import Data.Time.Calendar(Day)
import qualified Data.Map as Map(empty, fromAscList, map, toList)
import List(isPrefixOf, isSuffixOf)
import Monad(filterM)
import System(getArgs)
import System.Directory(createDirectoryIfMissing, copyFile, doesDirectoryExist, doesFileExist, getDirectoryContents)
import System.FilePath(combine, dropExtension, makeRelative, replaceDirectory)
import Text.ParserCombinators.Parsec(ParseError)
import Text.StringTemplate(directoryGroup, getStringTemplate, setManyAttrib, STGroup, StringTemplate, stShowsToSE, toString)
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

instance ToSElem TeamResult where
    toSElem result = SM $ Map.fromAscList[("conceded", toSElem $ conceded result),
                                          ("day", toSElem $ day result),
                                          ("opposition", toSElem $ opposition result),
                                          ("outcome", toSElem $ outcome result),
                                          ("scored", toSElem $ scored result),
                                          ("venue", toSElem $ venue result)]

-- | Existential quantified type for template values so that we can set multiple heterogenous attributes simultanteously.
data AttributeValue = forall a.(ToSElem a) => AV a
instance ToSElem AttributeValue where
    toSElem (AV a) = toSElem a

-- | Returns a list of files (excluding sub-directories and hidden files)  in the specified directory.  The returned paths
--   are fully-qualified.
getFiles :: FilePath -> IO [FilePath]
getFiles dir = do contents <- getDirectoryContents dir
                  let visible = filter (not . isPrefixOf ".") contents -- Exclude hidden files/directories.
                      absolute = map (combine dir) visible -- Use qualified paths.
                  files <- filterM (doesFileExist) absolute -- Exclude directories.
                  return files

-- | Returns a list of sub-directories (excluding those that are hidden)  in the specified directory.  The returned paths
--   are fully-qualified.
getSubDirectories :: FilePath -> IO [FilePath]
getSubDirectories dir = do contents <- getDirectoryContents dir
                           let visible = filter (not . isPrefixOf ".") contents -- Exclude hidden files/directories.
                               absolute = map (combine dir) visible -- Use qualified paths.
                           directories <- filterM (doesDirectoryExist) absolute -- Exclude files (non-directories).
                           return directories

-- | Copies all non-template files from the source directory to the target directory.  Used for making sure that CSS
--   files and images (if any) are deployed with the generated HTML.  If the target directory does not exist it is
--   created.
copyResources :: FilePath -> FilePath -> IO ()
copyResources from to = do files <- getFiles from
                           let resources = filter (not . isSuffixOf ".st") files
                           createDirectoryIfMissing True to
                           mapM_ (copyToDirectory to) resources

-- | Copies an individual file to a new directory, retaining the original file name.
copyToDirectory :: FilePath -> FilePath -> IO()
copyToDirectory dir file = copyFile file (replaceDirectory file dir)

-- | Generates an output file by applying a template with one or more attributes set.
applyTemplate :: STGroup String -> FilePath -> FilePath -> [(String, AttributeValue)] -> IO ()
applyTemplate group templateName dir attributes = case getStringTemplate templateName group of
                                                      Nothing       -> print $ "Could not find template for " ++ templateName
                                                      Just template -> writeFile (combine dir templateName) html
                                                                       where html = toString $ setManyAttrib attributes template

-- | Generates home, away and overall HTML league tables.
generateLeagueTables :: STGroup String -> FilePath -> Map Team [Result] -> Map Team Int -> IO ()
generateLeagueTables group dir results adjustments = do applyTemplate group "overalltable.html" dir [("table", AV $ leagueTable results adjustments)]
                                                        let splitResults = splitHomeAndAway results
                                                        applyTemplate group "hometable.html" dir [("table", AV $ leagueTable (Map.map fst splitResults) Map.empty)]
                                                        applyTemplate group "awaytable.html" dir [("table", AV $ leagueTable (Map.map snd splitResults) Map.empty)]

generateFormTables :: STGroup String -> FilePath -> Map Team [Result] -> IO ()
generateFormTables group dir results = do applyTemplate group "overallformtable.html" dir [("table", AV $ formTable results 6)]
                                          let splitResults = splitHomeAndAway results
                                          applyTemplate group "homeformtable.html" dir [("table", AV $ formTable (Map.map fst splitResults) 4)]
                                          applyTemplate group "awayformtable.html" dir [("table", AV $ formTable (Map.map snd splitResults) 4)]

generateResultsList :: STGroup String -> FilePath -> Map Day [Result] -> IO ()
generateResultsList group dir results = applyTemplate group "results.html" dir [("results", AV $ Map.toList results)]

generateSequences :: STGroup String -> FilePath -> Map Team [Result] -> IO ()
generateSequences group dir results = do let (overallCurrent, overallLongest) = getSequences results
                                             splitResults = splitHomeAndAway results
                                             (homeCurrent, homeLongest) = getSequences $ Map.map fst splitResults
                                             (awayCurrent, awayLongest) = getSequences $ Map.map snd splitResults
                                         applyTemplate group "overallcurrentsequences.html" dir [("sequences", AV $ sequenceTables overallCurrent)]
                                         applyTemplate group "overalllongestsequences.html" dir [("sequences", AV $ sequenceTables overallLongest)]
                                         applyTemplate group "homecurrentsequences.html" dir [("sequences", AV $ sequenceTables homeCurrent)]
                                         applyTemplate group "homelongestsequences.html" dir [("sequences", AV $ sequenceTables homeLongest)]
                                         applyTemplate group "awaycurrentsequences.html" dir [("sequences", AV $ sequenceTables awayCurrent)]
                                         applyTemplate group "awaylongestsequences.html" dir [("sequences", AV $ sequenceTables awayLongest)]

generateMiniLeagues :: STGroup String -> FilePath -> Map Team [Result] -> [(String, Set Team)] -> IO ()
generateMiniLeagues group dir results miniLeagues = mapM_ (generateMiniLeague group dir results) miniLeagues

generateMiniLeague :: STGroup String -> FilePath -> Map Team [Result] -> (String, Set Team) -> IO ()
generateMiniLeague group dir results (name, teams) = do applyTemplate group "minileague.html" dir [("table", AV $ miniLeague teams results), ("name", AV $ name)]

-- | Generates all stats pages for a given data file.  First parameter is a template group, second parameter is a pair of paths,
--   the first is the path to the data file, the second is the path to the directory in which the pages will be created.
generateStatsPages :: STGroup String -> FilePath -> [Result] -> Map Team Int -> [(String, Set Team)] -> IO ()
generateStatsPages templateGroup targetDir results adjustments miniLeagues = do let teamResults = resultsByTeam results
                                                                                createDirectoryIfMissing True targetDir
                                                                                generateLeagueTables templateGroup targetDir teamResults adjustments
                                                                                generateFormTables templateGroup targetDir teamResults
                                                                                generateResultsList templateGroup targetDir (resultsByDate results)
                                                                                generateSequences templateGroup targetDir teamResults
                                                                                generateMiniLeagues templateGroup targetDir teamResults miniLeagues 

-- Searches a directory and all of its sub-directories for data files.
findDataFiles :: FilePath -> IO [FilePath]
findDataFiles dir = do files <- getFiles dir
                       let dataFiles = filter (isSuffixOf ".rlt") files
                       directories <- getSubDirectories dir
                       subFiles <- mapM (findDataFiles) directories
                       return (dataFiles ++ concat subFiles)

processDataFile :: FilePath -> FilePath -> STGroup String -> FilePath -> IO ()
processDataFile dataDir outputDir templateGroup dataFile = do putStr $ "Processing " ++ dataFile ++ "\n"
                                                              (teams, results, adjustments, miniLeagues) <- parseRLTFile dataFile
                                                              let targetDir = combine outputDir (dropExtension $ makeRelative dataDir dataFile)
                                                              generateStatsPages templateGroup targetDir results adjustments miniLeagues

-- | Expects three arguments - the path to the RLT data files, the path to the templates directory and the path to the
--   output directory.
main :: IO ()
main = do dataDir:templateDir:outputDir:_ <- getArgs
          dataFiles <- findDataFiles dataDir
          group <- directoryGroup templateDir :: IO (STGroup String)
          copyResources templateDir outputDir
          mapM_ (processDataFile dataDir outputDir group) dataFiles

