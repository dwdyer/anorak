{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}

-- | HTML publishing module for the Anorak system.
module Anorak.Publisher (copyResources, publishLeagues) where

import Anorak.Config
import Anorak.Results
import Anorak.RLTParser
import Anorak.Sequences
import Anorak.Aggregates
import Anorak.Tables
import Anorak.Goals
import Anorak.Utils
import Control.Monad(filterM, unless)
import Data.ByteString.Char8(ByteString)
import qualified Data.ByteString.Char8 as BS(append, filter, map, unpack, writeFile)
import Data.Char(isSpace, toLower)
import Data.Data(Data)
import Data.List(foldl', isPrefixOf, isSuffixOf, nub)
import Data.Map(Map, (!))
import qualified Data.Map as Map(alter, assocs, empty, fromAscList, insert, keys, map, mapKeys, size, toDescList)
import Data.Sequence(Seq)
import Data.Set(Set)
import qualified Data.Set as Set(toList)
import Data.Time.Calendar(Day)
import Data.Typeable(Typeable)
import System.Directory(createDirectoryIfMissing, doesFileExist, getDirectoryContents)
import System.FilePath(combine)
import Text.StringTemplate(getStringTemplate, render, setManyAttrib, STGroup, stShowsToSE)
import Text.StringTemplate.Classes(ToSElem(toSElem), SElem(SM, STR))
import Text.StringTemplate.GenericStandard()

instance ToSElem LeagueRecord where
    toSElem record = SM $ Map.fromAscList [("adjustment", toSElem . adjustmentString $ adjustment record),
                                           ("against", toSElem $ against record),
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
    toSElem result = SM $ Map.fromAscList [("awayGoals", toSElem . reduceScorers $ awayGoals result),
                                           ("awayScore", toSElem $ awayScore result),
                                           ("awayTeam", toSElem $ awayTeam result),
                                           ("date", toSElem $ date result),
                                           ("homeGoals", toSElem.reduceScorers $ homeGoals result),
                                           ("homeScore", toSElem $ homeScore result),
                                           ("homeTeam", toSElem $ homeTeam result)]

instance ToSElem TeamResult where
    toSElem result = SM $ Map.fromAscList [("conceded", toSElem $ conceded result),
                                           ("day", toSElem $ day result),
                                           ("goals", toSElem . reduceScorers $ goals result),
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
                          isCollated :: Bool,
                          hasScorers :: Bool,
                          teamLinks :: Map String String,
                          miniLeaguesLink :: Maybe String}
instance ToSElem MetaData where
    toSElem meta = SM $ Map.fromAscList [("division", STR $ division meta),
                                         ("hasScorers", toSElem $ hasScorers meta),
                                         ("isAggregated", toSElem $ isAggregated meta),
                                         ("isCollated", toSElem $ isCollated meta),
                                         ("league", STR $ league meta),
                                         ("miniLeaguesLink", toSElem $ miniLeaguesLink meta),
                                         ("season", STR $ season meta),
                                         ("teamLinks", SM . Map.map toSElem $ teamLinks meta)]

-- | Convert points adjustment into a string with +/- sign, or Nothing if there is no adjustment.
adjustmentString :: Int -> Maybe String
adjustmentString adj
    | adj < 0   = Just $ show adj
    | adj > 0   = Just $ '+' : show adj
    | otherwise = Nothing

-- | If a player has scored more than one goal in the game, combine those goals into a single entry.
--   Returns a list of pairs, first item is the player's name, second is a string containing details of their goals.
reduceScorers :: [Goal] -> [(ByteString, String)]
reduceScorers goals = map (\s -> (s, reducedMap!s)) scorers
                      where reducedMap = foldl' addGoalToScorers Map.empty goals
                            scorers = nub $ map scorer goals

addGoalToScorers :: Map ByteString String -> Goal -> Map ByteString String
addGoalToScorers scorers goal = Map.alter (updateScorer goal) (scorer goal) scorers
                                where updateScorer goal Nothing  = Just $ goalTypeString goal ++ show (minute goal)
                                      updateScorer goal (Just d) = Just $ d ++ ", " ++ goalTypeString goal ++ show (minute goal)

goalTypeString :: Goal -> String
goalTypeString goal = case goalType goal of
                          "p" -> "pen "
                          "o" -> "o.g. "
                          _   -> ""

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
applyTemplate :: STGroup ByteString -> FilePath -> FilePath -> [(String, AttributeValue)] -> IO ()
applyTemplate group templateName dir = applyTemplateWithName group templateName dir templateName

applyTemplateWithName :: STGroup ByteString -> FilePath -> FilePath -> FilePath -> [(String, AttributeValue)] -> IO ()
applyTemplateWithName group templateName dir fileName attributes = case getStringTemplate templateName group of
                                                                        Nothing       -> print $ "Could not find template for " ++ templateName
                                                                        Just template -> BS.writeFile (combine dir fileName) html
                                                                                         where html = render $ setManyAttrib attributes template

-- | Generates home, away and overall HTML league tables.
generateLeagueTables :: STGroup ByteString -> FilePath -> Results -> Map Team Int -> MetaData -> Int -> IO ()
generateLeagueTables group dir results adjustments metaData sp = do let attributes = [("tableSelected", AV True), ("metaData", AV metaData)]
                                                                    applyTemplateWithName group "table.html" dir "index.html" (("table", AV $ leagueTable (byTeam results) adjustments sp):attributes)
                                                                    applyTemplate group "hometable.html" dir (("table", AV $ leagueTable (homeOnly results) Map.empty 0):attributes)
                                                                    applyTemplate group "awaytable.html" dir (("table", AV $ leagueTable (awayOnly results) Map.empty 0):attributes)
                                                                    unless (not $ hasScorers metaData) $ applyTemplate group "firsthalftable.html" dir (("table", AV $ leagueTable (firstHalf results) Map.empty 0):attributes)
                                                                    unless (not $ hasScorers metaData) $ applyTemplate group "secondhalftable.html" dir (("table", AV $ leagueTable (secondHalf results) Map.empty 0):attributes)

generateFormTables :: STGroup ByteString -> FilePath -> Results -> MetaData -> IO ()
generateFormTables group dir results metaData = do let attributes = [("formSelected", AV True), ("metaData", AV metaData)]
                                                   applyTemplate group "formtable.html" dir (("table", AV $ formTable (byTeam results) 6):attributes)
                                                   applyTemplate group "homeformtable.html" dir (("table", AV $ formTable (homeOnly results) 4):attributes)
                                                   applyTemplate group "awayformtable.html" dir (("table", AV $ formTable (awayOnly results) 4):attributes)

-- | Generates current and longest sequences for home, away and all matches.
generateSequences :: STGroup ByteString -> FilePath -> Results -> MetaData -> IO ()
generateSequences group dir results metaData = do let (overallCurrent, overallLongest) = getSequenceTables $ byTeam results
                                                      (homeCurrent, homeLongest) = getSequenceTables $ homeOnly results
                                                      (awayCurrent, awayLongest) = getSequenceTables $ awayOnly results
                                                      attributes = [("metaData", AV metaData)]
                                                  applyTemplate group "currentsequences.html" dir (("sequences", convertTables overallCurrent):("currentSequencesSelected", AV True):attributes)
                                                  applyTemplate group "longestsequences.html" dir (("sequences", convertTables overallLongest):("longestSequencesSelected", AV True):attributes)
                                                  applyTemplate group "homecurrentsequences.html" dir (("sequences", convertTables homeCurrent):("currentSequencesSelected", AV True):attributes)
                                                  applyTemplate group "homelongestsequences.html" dir (("sequences", convertTables homeLongest):("longestSequencesSelected", AV True):attributes)
                                                  applyTemplate group "awaycurrentsequences.html" dir (("sequences", convertTables awayCurrent):("currentSequencesSelected", AV True):attributes)
                                                  applyTemplate group "awaylongestsequences.html" dir (("sequences", convertTables awayLongest):("longestSequencesSelected", AV True):attributes)
                                                  where convertTables = AV . Map.mapKeys show

-- | Generates team aggregates for all matches.
generateAggregates:: STGroup ByteString -> FilePath -> Results -> MetaData -> IO ()
generateAggregates group dir results metaData = do let aggregates = getAggregateTables $ byTeam results
                                                       attributes = [("metaData", AV metaData)]
                                                   applyTemplate group "aggregates.html" dir (("aggregates", AV . Map.mapKeys show $ aggregates):("aggregatesSelected", AV True):attributes)

generateResults :: STGroup ByteString -> FilePath -> Results -> MetaData -> IO ()
generateResults group dir results metaData = do let homeWinMatches = homeWins $ list results
                                                    awayWinMatches = awayWins $ list results
                                                    matchCount = length $ list results
                                                    homeWinCount = length homeWinMatches
                                                    awayWinCount = length awayWinMatches
                                                    drawCount = matchCount - homeWinCount - awayWinCount
                                                    goalCount = sum $ map aggregate $ list results
                                                    highAggregates = highestAggregates (list results)
                                                applyTemplate group "results.html" dir [("results", AV . Map.toDescList $ byDate results),
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

generateMiniLeagues :: STGroup ByteString -> FilePath -> Results -> [(ByteString, Set Team)] -> MetaData -> IO ()
generateMiniLeagues group dir results miniLeagues metaData = do let tabs = map ((\n -> (n, toHTMLFileName n)).fst) miniLeagues -- Each tab is a display name and a file name.
                                                                mapM_ (generateMiniLeague group dir results tabs metaData) miniLeagues

generateMiniLeague :: STGroup ByteString -> FilePath -> Results -> [(ByteString, String)] -> MetaData -> (ByteString, Set Team) -> IO ()
generateMiniLeague group dir results tabs metaData (name, teams) = do let selectedTabs = map (\(n, f) -> (n, f, n == name)) tabs -- Add a boolean "selected" flag to each tab.
                                                                          attributes = [("table", AV $ miniLeagueTable teams $ byTeam results),
                                                                                        ("miniLeaguesSelected", AV True),
                                                                                        ("name", AV name),
                                                                                        ("bottomTabs", AV selectedTabs),
                                                                                        ("metaData", AV metaData)]
                                                                      applyTemplateWithName group "minileague.html" dir (toHTMLFileName name) attributes

generateTeamPages :: STGroup ByteString -> FilePath -> Results -> Map Team [(Day, Int)] -> MetaData -> IO ()
generateTeamPages group dir results positions metaData = mapM_ (\(team, res) -> generateTeamPage group dir team res (positions ! team) metaData) $ Map.assocs $ byTeam results

-- | Generate the overview page for an individual team.
generateTeamPage :: STGroup ByteString -> FilePath -> Team -> [Result] -> [(Day, Int)] -> MetaData -> IO ()
generateTeamPage group dir team results positions metaData = do let (homeResults, awayResults) = partitionResults team results
                                                                    teamResults = map (convertResult team) results
                                                                    (goalScorers, ownGoals) = teamGoalScorers teamResults
                                                                    attributes = [("team", AV team),
                                                                                  ("results", AV teamResults),
                                                                                  ("record", AV $ getSummary team results),
                                                                                  ("homeRecord", AV $ getSummary team homeResults),
                                                                                  ("awayRecord", AV $ getSummary team awayResults),
                                                                                  ("scorers", AV goalScorers),
                                                                                  ("ownGoals", AV $ show ownGoals),
                                                                                  ("positions", AV positions),
                                                                                  ("teamCount", AV . Map.size $ teamLinks metaData),
                                                                                  ("metaData", AV metaData)]
                                                                applyTemplateWithName group "team.html" dir (teamLinks metaData ! BS.unpack team) attributes

getSummary :: Team -> [Result] -> (Int, Float, Int, Float, Int, Float)
getSummary team results = (won record,
                           percentage (won record) matches,
                           drawn record,
                           percentage (drawn record) matches,
                           lost record,
                           percentage (lost record) matches)
                          where record = buildRecord team results
                                matches = played record

-- | Generates the top scorers list (only if there are scorers in the data).
generateGoals:: STGroup ByteString -> FilePath -> Results -> MetaData -> IO ()
generateGoals group dir results metaData = do let scorers = topGoalScorers $ list results
                                                  penalties = topPenaltyScorers $ list results
                                                  attributes = [("metaData", AV metaData)]
                                              unless (null scorers) $ applyTemplate group "goals.html" dir (("scorers", AV scorers):("penalties", AV penalties):("goalsSelected", AV True):attributes)

-- | Convert a string for use as a filename (converts to lower case and eliminates whitespace).
toHTMLFileName :: ByteString -> String
toHTMLFileName name = BS.unpack $ BS.map toLower (BS.filter (not.isSpace) name) `BS.append` ".html"

-- | Take a list of team names and return mappings for the associated team pages.
mapTeamNames :: [Team] -> Map String String
mapTeamNames = foldl' (\m t -> Map.insert (BS.unpack t) (toHTMLFileName t) m) Map.empty

-- | Generates all stats pages for a given data file.  First parameter is a template group, second parameter is a pair of paths,
--   the first is the path to the data file, the second is the path to the directory in which the pages will be created.
generateStatsPages :: STGroup ByteString -> FilePath -> LeagueData -> MetaData -> IO ()
generateStatsPages templateGroup targetDir (LeagueData teams res adj miniLeagues sp) metaData = do let results = prepareResults res
                                                                                                       positions = leaguePositions teams (byDate results) adj
                                                                                                   createDirectoryIfMissing True targetDir
                                                                                                   generateLeagueTables templateGroup targetDir results adj metaData sp
                                                                                                   generateFormTables templateGroup targetDir results metaData
                                                                                                   generateResults templateGroup targetDir results metaData
                                                                                                   generateSequences templateGroup targetDir results metaData
                                                                                                   generateAggregates templateGroup targetDir results metaData
                                                                                                   generateMiniLeagues templateGroup targetDir results miniLeagues metaData
                                                                                                   generateTeamPages templateGroup targetDir results positions metaData
                                                                                                   unless (not $ hasScorers metaData) $ generateGoals templateGroup targetDir results metaData

-- | Determine which file the "Mini-Leagues" tab should link to (derived from the name of the first mini-league).
--   If there are no mini-leagues then this function returns nothing and the tab should not be shown.
getMiniLeaguesLink :: [(ByteString, Set Team)] -> Maybe String
getMiniLeaguesLink []            = Nothing
getMiniLeaguesLink ((name, _):_) = Just $ toHTMLFileName name

publishLeagues :: STGroup ByteString -> Configuration -> IO ()
publishLeagues templates config = do applyTemplate templates "selector.json" (outputRoot config) [("config", AV config)]
                                     mapM_ (publishLeague templates) $ leagues config

publishLeague :: STGroup ByteString -> League -> IO ()
publishLeague templates league = mapM_ (publishDivision templates (leagueName league)) $ divisions league

publishDivision :: STGroup ByteString -> String -> Division -> IO ()
publishDivision templateGroup leagueName division = mapM_ (publishSeason templateGroup leagueName (divisionName division)) $ seasons division

publishSeason :: STGroup ByteString -> String -> String -> Season -> IO ()
publishSeason templates lgName divName season = do let dataFile = inputFile season
                                                   modified <- isNewer dataFile (combine (outputDir season) "index.html")
                                                   case modified || aggregated season || collated season of
                                                       False -> print $ "Skipping unchanged file " ++ dataFile 
                                                       True  -> do print $ "Processing " ++ dataFile
                                                                   leagueData@(LeagueData teams results _ miniLeagues _) <- parseRLTFile dataFile
                                                                   let teamLinks = mapTeamNames $ Set.toList teams
                                                                       metaData = MetaData lgName
                                                                                           divName
                                                                                           (seasonName season)
                                                                                           (aggregated season)
                                                                                           (collated season)
                                                                                           (scorers season)
                                                                                           teamLinks
                                                                                           (getMiniLeaguesLink miniLeagues)
                                                                   generateStatsPages templates (outputDir season) leagueData metaData 

