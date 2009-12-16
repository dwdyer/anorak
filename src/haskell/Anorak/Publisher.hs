{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ExistentialQuantification #-}

-- | HTML publishing module for the Anorak system.
module Anorak.Publisher (copyResources, publishLeagues) where

import Anorak.Config
import Anorak.Results
import Anorak.RLTParser
import Anorak.Sequences
import Anorak.Tables
import Anorak.Utils
import Char(isSpace, toLower)
import Data.Data(Data)
import Data.Map(Map)
import qualified Data.Map as Map(assocs, empty, fromAscList, map, toList)
import Data.Set(Set)
import Data.Typeable(Typeable)
import List(isPrefixOf, isSuffixOf)
import Monad(filterM)
import System.Directory(createDirectoryIfMissing, doesFileExist, getDirectoryContents)
import System.FilePath(combine)
import Text.StringTemplate(getStringTemplate, setManyAttrib, STGroup, stShowsToSE, toString)
import Text.StringTemplate.Classes(ToSElem(toSElem), SElem(SM, STR))
import Text.StringTemplate.GenericStandard()

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
                                           ("team", STR $ team record),
                                           ("teamLink", STR . toHTMLFileName $ team record),
                                           ("won", toSElem $ won record)]
instance ToSElem Result where
    toSElem result = SM $ Map.fromAscList [("awayGoals", toSElem $ awayGoals result),
                                           ("awayTeam", toSElem $ awayTeam result),
                                           ("date", toSElem $ date result),
                                           ("homeGoals", toSElem $ homeGoals result),
                                           ("homeTeam", toSElem $ homeTeam result)]

instance ToSElem TeamResult where
    toSElem result = SM $ Map.fromAscList [("conceded", toSElem $ conceded result),
                                           ("day", toSElem $ day result),
                                           ("opposition", toSElem $ opposition result),
                                           ("outcome", toSElem $ outcome result),
                                           ("scored", toSElem $ scored result),
                                           ("venue", toSElem $ venue result)]

-- | Existential quantified type for template values so that we can set multiple heterogenous attributes simultanteously.
data AttributeValue = forall a.(ToSElem a) => AV a
instance ToSElem AttributeValue where
    toSElem (AV a) = toSElem a

-- | Data about the season being published, useful in templates.
data MetaData = MetaData {league :: String,
                          division :: String,
                          season :: String,
                          isAggregated :: Bool,
                          miniLeaguesLink :: Maybe String} deriving (Data, Typeable)

-- | Returns a list of files (excluding sub-directories and hidden files)  in the specified directory.  The returned paths
--   are fully-qualified.
getFiles :: FilePath -> IO [FilePath]
getFiles dir = do contents <- getDirectoryContents dir
                  let visible = filter (not . isPrefixOf ".") contents -- Exclude hidden files/directories.
                      absolute = map (combine dir) visible -- Use qualified paths.
                  filterM doesFileExist absolute -- Exclude directories.

-- | Copies all non-template files from the source directory to the target directory.  Used for making sure that CSS
--   files and images (if any) are deployed with the generated HTML.  If the target directory does not exist it is
--   created.
copyResources :: FilePath -> FilePath -> IO ()
copyResources from to = do files <- getFiles from
                           let resources = filter (not . isSuffixOf ".st") files
                           createDirectoryIfMissing True to
                           mapM_ (copyToDirectory to) resources

-- | Generates an output file by applying a template with one or more attributes set.  The file's name is derived
--   from the template name.
applyTemplate :: STGroup String -> FilePath -> FilePath -> [(String, AttributeValue)] -> IO ()
applyTemplate group templateName dir = applyTemplateWithName group templateName dir templateName

applyTemplateWithName :: STGroup String -> FilePath -> FilePath -> FilePath -> [(String, AttributeValue)] -> IO ()
applyTemplateWithName group templateName dir fileName attributes = case getStringTemplate templateName group of
                                                                        Nothing       -> print $ "Could not find template for " ++ templateName
                                                                        Just template -> writeFile (combine dir fileName) html
                                                                                         where html = toString $ setManyAttrib attributes template

-- | Generates home, away and overall HTML league tables.
generateLeagueTables :: STGroup String -> FilePath -> Map Team [Result] -> Map Team Int -> MetaData -> IO ()
generateLeagueTables group dir results adjustments metaData = do let splitResults = splitHomeAndAway results
                                                                     attributes = [("tableSelected", AV True), ("metaData", AV metaData)]
                                                                 applyTemplateWithName group "table.html" dir "index.html" (("table", AV $ leagueTable results adjustments):attributes)
                                                                 applyTemplate group "hometable.html" dir (("table", AV $ leagueTable (Map.map fst splitResults) Map.empty):attributes)
                                                                 applyTemplate group "awaytable.html" dir (("table", AV $ leagueTable (Map.map snd splitResults) Map.empty):attributes)

generateFormTables :: STGroup String -> FilePath -> Map Team [Result] -> MetaData -> IO ()
generateFormTables group dir results metaData = do let splitResults = splitHomeAndAway results
                                                       attributes = [("formSelected", AV True), ("metaData", AV metaData)]
                                                   applyTemplate group "formtable.html" dir (("table", AV $ formTable results 6):attributes)
                                                   applyTemplate group "homeformtable.html" dir (("table", AV $ formTable (Map.map fst splitResults) 4):attributes)
                                                   applyTemplate group "awayformtable.html" dir (("table", AV $ formTable (Map.map snd splitResults) 4):attributes)

-- | Generates current and longest sequences for home, away and all matches.
generateSequences :: STGroup String -> FilePath -> Map Team [Result] -> MetaData -> IO ()
generateSequences group dir results metaData = do let (overallCurrent, overallLongest) = getSequences results
                                                      splitResults = splitHomeAndAway results
                                                      (homeCurrent, homeLongest) = getSequences $ Map.map fst splitResults
                                                      (awayCurrent, awayLongest) = getSequences $ Map.map snd splitResults
                                                      attr = ("metaData", AV metaData)
                                                  applyTemplate group "currentsequences.html" dir [("sequences", AV $ sequenceTables overallCurrent), ("currentSequencesSelected", AV True), attr]
                                                  applyTemplate group "longestsequences.html" dir [("sequences", AV $ sequenceTables overallLongest), ("longestSequencesSelected", AV True), attr]
                                                  applyTemplate group "homecurrentsequences.html" dir [("sequences", AV $ sequenceTables homeCurrent), ("currentSequencesSelected", AV True), attr]
                                                  applyTemplate group "homelongestsequences.html" dir [("sequences", AV $ sequenceTables homeLongest), ("longestSequencesSelected", AV True), attr]
                                                  applyTemplate group "awaycurrentsequences.html" dir [("sequences", AV $ sequenceTables awayCurrent), ("currentSequencesSelected", AV True), attr]
                                                  applyTemplate group "awaylongestsequences.html" dir [("sequences", AV $ sequenceTables awayLongest), ("longestSequencesSelected", AV True), attr]

generateResults :: STGroup String -> FilePath -> [Result] -> MetaData -> IO ()
generateResults group dir results metaData = do let homeWinMatches = homeWins results
                                                    awayWinMatches = awayWins results
                                                    matchCount = length results
                                                    homeWinCount = length homeWinMatches
                                                    awayWinCount = length awayWinMatches
                                                    drawCount = matchCount - homeWinCount - awayWinCount
                                                    goalCount = sum $ map aggregate results
                                                    highAggregates = highestAggregates results
                                                applyTemplate group "results.html" dir [("results", AV $ reverse . Map.toList $ resultsByDate results), -- Reverse to list most recent first.
                                                                                        ("matches", AV matchCount),
                                                                                        ("homeWins", AV homeWinCount),
                                                                                        ("homeWinPercent", AV $ percentage homeWinCount matchCount),
                                                                                        ("awayWins", AV awayWinCount),
                                                                                        ("awayWinPercent", AV $ percentage awayWinCount matchCount),
                                                                                        ("draws", AV drawCount),
                                                                                        ("drawPercent", AV $ percentage drawCount matchCount),
                                                                                        ("goals", AV goalCount),
                                                                                        ("bigHomeWins", AV $ biggestWins homeWinMatches),
                                                                                        ("bigAwayWins", AV $ biggestWins awayWinMatches),
                                                                                        ("highAggregates", AV highAggregates),
                                                                                        ("resultsSelected", AV True),
                                                                                        ("metaData", AV metaData)]

generateMiniLeagues :: STGroup String -> FilePath -> Map Team [Result] -> [(String, Set Team)] -> MetaData -> IO ()
generateMiniLeagues group dir results miniLeagues metaData = do let tabs = map ((\n -> (n, toHTMLFileName n)).fst) miniLeagues -- Each tab is a display name and a file name.
                                                                mapM_ (generateMiniLeague group dir results tabs metaData) miniLeagues

generateMiniLeague :: STGroup String -> FilePath -> Map Team [Result] -> [(String, String)] -> MetaData -> (String, Set Team) -> IO ()
generateMiniLeague group dir results tabs metaData (name, teams) = do let selectedTabs = map (\(n, f) -> (n, f, (n == name))) tabs -- Add a boolean "selected" flag to each tab.
                                                                          attributes = [("table", AV $ miniLeagueTable teams results),
                                                                                        ("miniLeaguesSelected", AV True),
                                                                                        ("name", AV name),
                                                                                        ("bottomTabs", AV selectedTabs),
                                                                                        ("metaData", AV metaData)]
                                                                      applyTemplateWithName group "minileague.html" dir (toHTMLFileName name) attributes

generateTeamPages :: STGroup String -> FilePath -> Map Team [Result] -> MetaData -> IO ()
generateTeamPages group dir teamResults metaData = mapM_ (uncurry $ generateTeamPage group dir metaData) (Map.assocs teamResults)

-- | Generate the overview page for an individual team.
generateTeamPage :: STGroup String -> FilePath -> MetaData -> Team -> [Result] -> IO ()
generateTeamPage group dir metaData team results = do let attributes = [("team", AV team),
                                                                        ("results", AV $ map (convertResult team) results),
                                                                        ("metaData", AV metaData)]
                                                      applyTemplateWithName group "team.html" dir (toHTMLFileName team) attributes

-- | Convert a string for use as a filename (converts to lower case and eliminates whitespace).
toHTMLFileName :: String -> String
toHTMLFileName name = map toLower (filter (not.isSpace) name) ++ ".html"

-- | Generates all stats pages for a given data file.  First parameter is a template group, second parameter is a pair of paths,
--   the first is the path to the data file, the second is the path to the directory in which the pages will be created.
generateStatsPages :: STGroup String -> FilePath -> LeagueData -> MetaData -> IO ()
generateStatsPages templateGroup targetDir (LeagueData _ results adj miniLeagues) metaData = do let teamResults = resultsByTeam results
                                                                                                createDirectoryIfMissing True targetDir
                                                                                                generateLeagueTables templateGroup targetDir teamResults adj metaData
                                                                                                generateFormTables templateGroup targetDir teamResults metaData
                                                                                                generateResults templateGroup targetDir results metaData
                                                                                                generateSequences templateGroup targetDir teamResults metaData
                                                                                                generateMiniLeagues templateGroup targetDir teamResults miniLeagues metaData
                                                                                                generateTeamPages templateGroup targetDir teamResults metaData

-- | Determine which file the "Mini-Leagues" tab should link to (derived from the name of the first mini-league).
--   If there are no mini-leagues then this function returns nothing and the tab should not be shown.
getMiniLeaguesLink :: [(String, Set Team)] -> Maybe String
getMiniLeaguesLink []            = Nothing
getMiniLeaguesLink ((name, _):_) = Just $ toHTMLFileName name

publishLeagues :: STGroup String -> Configuration -> IO ()
publishLeagues templateGroup config = do applyTemplate templateGroup "selector.json" (outputRoot config) [("config", AV config)]
                                         mapM_ (publishLeague templateGroup) $ leagues config

publishLeague :: STGroup String -> League -> IO ()
publishLeague templateGroup league = mapM_ (publishDivision templateGroup (leagueName league)) $ divisions league

publishDivision :: STGroup String -> String -> Division -> IO ()
publishDivision templateGroup leagueName division = mapM_ (publishSeason templateGroup leagueName (divisionName division)) $ seasons division

publishSeason :: STGroup String -> String -> String -> Season -> IO ()
publishSeason templateGroup leagueName divisionName season = do let dataFile = inputFile season
                                                                print $ "Processing " ++ dataFile
                                                                LeagueData teams results adj miniLeagues <- parseRLTFile dataFile
                                                                let metaData = MetaData leagueName divisionName (seasonName season) (aggregated season) (getMiniLeaguesLink miniLeagues)
                                                                generateStatsPages templateGroup (outputDir season) (LeagueData teams results adj miniLeagues) metaData 

