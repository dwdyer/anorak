{-# LANGUAGE ExistentialQuantification #-}

-- | HTML publishing module for the Anorak system.
module Anorak.Publisher (copyResources, findDataFiles, processDataFile) where

import Anorak.Core
import Anorak.Sequences
import Anorak.Tables
import Anorak.Types
import Anorak.RLTParser
import Char(isSpace, toLower)
import Data.Map(Map)
import Data.Set(Set)
import Data.Time.Calendar(Day)
import qualified Data.Map as Map(empty, fromAscList, map, toList)
import List(isPrefixOf, isSuffixOf)
import Monad(filterM)
import System.Directory(createDirectoryIfMissing, copyFile, doesDirectoryExist, doesFileExist, getDirectoryContents)
import System.FilePath(combine, dropExtension, makeRelative, replaceDirectory)
import Text.ParserCombinators.Parsec(ParseError)
import Text.StringTemplate(getStringTemplate, setManyAttrib, STGroup, StringTemplate, stShowsToSE, toString)
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

-- | Generates an output file by applying a template with one or more attributes set.  The file's name is derived
--   from the template name.
applyTemplate :: STGroup String -> FilePath -> FilePath -> [(String, AttributeValue)] -> IO ()
applyTemplate group templateName dir attributes = applyTemplateWithName group templateName dir templateName attributes 

applyTemplateWithName :: STGroup String -> FilePath -> FilePath -> FilePath -> [(String, AttributeValue)] -> IO ()
applyTemplateWithName group templateName dir fileName attributes = case getStringTemplate templateName group of
                                                                        Nothing       -> print $ "Could not find template for " ++ templateName
                                                                        Just template -> writeFile (combine dir fileName) html
                                                                                         where html = toString $ setManyAttrib attributes template

-- | Generates home, away and overall HTML league tables.
generateLeagueTables :: STGroup String -> FilePath -> Map Team [Result] -> Map Team Int -> Maybe String -> IO ()
generateLeagueTables group dir results adjustments link = do let splitResults = splitHomeAndAway results
                                                                 attributes = [("tableSelected", AV True), ("miniLeaguesLink", AV link)]
                                                             applyTemplate group "overalltable.html" dir (("table", AV $ leagueTable results adjustments):attributes)
                                                             applyTemplate group "hometable.html" dir (("table", AV $ leagueTable (Map.map fst splitResults) Map.empty):attributes)
                                                             applyTemplate group "awaytable.html" dir (("table", AV $ leagueTable (Map.map snd splitResults) Map.empty):attributes)

generateFormTables :: STGroup String -> FilePath -> Map Team [Result] -> Maybe String -> IO ()
generateFormTables group dir results link = do let splitResults = splitHomeAndAway results
                                                   attributes = [("formSelected", AV True), ("miniLeaguesLink", AV link)]
                                               applyTemplate group "overallformtable.html" dir (("table", AV $ formTable results 6):attributes)
                                               applyTemplate group "homeformtable.html" dir (("table", AV $ formTable (Map.map fst splitResults) 4):attributes)
                                               applyTemplate group "awayformtable.html" dir (("table", AV $ formTable (Map.map snd splitResults) 4):attributes)

generateResultsList :: STGroup String -> FilePath -> Map Day [Result] -> Maybe String -> IO ()
generateResultsList group dir results link = applyTemplate group "results.html" dir [("results", AV $ reverse $ Map.toList results), -- Reverse to list most recent first.
                                                                                     ("resultsSelected", AV True),
                                                                                     ("miniLeaguesLink", AV link)]

-- | Generates current and longest sequences for home, away and all matches.
generateSequences :: STGroup String -> FilePath -> Map Team [Result] -> Maybe String -> IO ()
generateSequences group dir results link = do let (overallCurrent, overallLongest) = getSequences results
                                                  splitResults = splitHomeAndAway results
                                                  (homeCurrent, homeLongest) = getSequences $ Map.map fst splitResults
                                                  (awayCurrent, awayLongest) = getSequences $ Map.map snd splitResults
                                                  linkAttr = ("miniLeaguesLink", AV link)
                                              applyTemplate group "overallcurrentsequences.html" dir [("sequences", AV $ sequenceTables overallCurrent), ("currentSequencesSelected", AV True), linkAttr]
                                              applyTemplate group "overalllongestsequences.html" dir [("sequences", AV $ sequenceTables overallLongest), ("longestSequencesSelected", AV True), linkAttr]
                                              applyTemplate group "homecurrentsequences.html" dir [("sequences", AV $ sequenceTables homeCurrent), ("currentSequencesSelected", AV True), linkAttr]
                                              applyTemplate group "homelongestsequences.html" dir [("sequences", AV $ sequenceTables homeLongest), ("longestSequencesSelected", AV True), linkAttr]
                                              applyTemplate group "awaycurrentsequences.html" dir [("sequences", AV $ sequenceTables awayCurrent), ("currentSequencesSelected", AV True), linkAttr]
                                              applyTemplate group "awaylongestsequences.html" dir [("sequences", AV $ sequenceTables awayLongest), ("longestSequencesSelected", AV True), linkAttr]

generateMiniLeagues :: STGroup String -> FilePath -> Map Team [Result] -> [(String, Set Team)] -> IO ()
generateMiniLeagues group dir results miniLeagues = do let tabs = map ((\n -> (n, reduceName n ++ ".html")).fst) miniLeagues -- Each tab is a display name and a file name.
                                                       mapM_ (generateMiniLeague group dir results tabs) miniLeagues

generateMiniLeague :: STGroup String -> FilePath -> Map Team [Result] -> [(String, String)] -> (String, Set Team) -> IO ()
generateMiniLeague group dir results tabs (name, teams) = do let selectedTabs = map (\(n, f) -> (n, f, (n == name))) tabs -- Add a boolean "selected" flag to each tab.
                                                                 attributes = [("table", AV $ miniLeagueTable teams results),
                                                                               ("miniLeaguesSelected", AV True),
                                                                               ("name", AV $ name),
                                                                               ("bottomTabs", AV $ selectedTabs)]
                                                             applyTemplateWithName group "minileague.html" dir (reduceName name ++ ".html") attributes
                                                             

-- | Convert a string for use as a filename (converts to lower case and eliminates whitespace).
reduceName :: String -> String
reduceName name = map toLower $ filter (not.isSpace) name

-- | Generates all stats pages for a given data file.  First parameter is a template group, second parameter is a pair of paths,
--   the first is the path to the data file, the second is the path to the directory in which the pages will be created.
generateStatsPages :: STGroup String -> FilePath -> LeagueData -> IO ()
generateStatsPages templateGroup targetDir (LeagueData _ results adjustments miniLeagues) = do let teamResults = resultsByTeam results
                                                                                                   link = miniLeaguesLink miniLeagues
                                                                                               createDirectoryIfMissing True targetDir
                                                                                               generateLeagueTables templateGroup targetDir teamResults adjustments link
                                                                                               generateFormTables templateGroup targetDir teamResults link
                                                                                               generateResultsList templateGroup targetDir (resultsByDate results) link
                                                                                               generateSequences templateGroup targetDir teamResults link
                                                                                               generateMiniLeagues templateGroup targetDir teamResults miniLeagues 

-- | Determine which file the "Mini-Leagues" tab should link to (derived from the name of the first mini-league).
--   If there are no mini-leagues then this function returns nothing and the tab should not be shown.
miniLeaguesLink :: [(String, Set Team)] -> Maybe String
miniLeaguesLink []             = Nothing
miniLeaguesLink ((name, _):ls) = Just $ (reduceName name) ++ ".html"

-- | Searches a directory and all of its sub-directories for data files.
findDataFiles :: FilePath -> IO [FilePath]
findDataFiles dir = do files <- getFiles dir
                       let dataFiles = filter (isSuffixOf ".rlt") files
                       directories <- getSubDirectories dir
                       subFiles <- mapM (findDataFiles) directories
                       return (dataFiles ++ concat subFiles)

processDataFile :: FilePath -> FilePath -> STGroup String -> FilePath -> IO ()
processDataFile dataDir outputDir templateGroup dataFile = do print $ "Processing " ++ dataFile
                                                              leagueData <- parseRLTFile dataFile
                                                              let targetDir = combine outputDir (dropExtension $ makeRelative dataDir dataFile)
                                                              generateStatsPages templateGroup targetDir leagueData


