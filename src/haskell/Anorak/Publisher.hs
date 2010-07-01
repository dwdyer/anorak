{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | HTML publishing module for the Anorak system.
module Anorak.Publisher (publishLeagues) where

import Anorak.Config
import Anorak.Results
import Anorak.RLTParser
import Anorak.Sequences
import Anorak.Aggregates
import Anorak.Tables
import Anorak.Goals
import Data.ByteString.Char8(ByteString)
import qualified Data.ByteString.Char8 as BS(append, filter, map, unpack, writeFile)
import Data.Char(isSpace, toLower)
import Data.List(foldl', groupBy, nub)
import Data.Map(Map, (!))
import qualified Data.Map as Map(alter, empty, findWithDefault, fromAscList, insert, keys, map, mapKeys, size, toDescList)
import Data.Set(Set)
import qualified Data.Set as Set(toList)
import Data.Time.Calendar(Day)
import System.Directory(createDirectoryIfMissing)
import System.FilePath((</>))
import Text.StringTemplate(getStringTemplate, render, setManyAttrib, STGroup, stShowsToSE)
import Text.StringTemplate.Classes(ToSElem(toSElem), SElem(SM))
import Text.StringTemplate.GenericStandard()
import Util.File(isNewer)
import Util.List(equal, takeAtLeast)
import Util.Maths(percentage, roundUp)
import Util.Tuple(pair)

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
                                           ("homeGoals", toSElem . reduceScorers $ homeGoals result),
                                           ("homeScore", toSElem $ homeScore result),
                                           ("homeTeam", toSElem $ homeTeam result)]

instance ToSElem TeamResult where
    toSElem result = SM $ Map.fromAscList [("conceded", toSElem $ conceded result),
                                           ("day", toSElem $ day result),
                                           ("goals", toSElem . reduceScorers $ goalsFor result),
                                           ("opposition", toSElem $ opposition result),
                                           ("outcome", toSElem $ outcome result),
                                           ("scored", toSElem $ scored result),
                                           ("venue", toSElem $ venue result)]

-- | Existential quantified type for template values so that we can set multiple heterogenous attributes simultanteously.
data AttributeValue = forall a.(ToSElem a) => AV a
instance ToSElem AttributeValue where
    toSElem (AV a) = toSElem a

-- | A page is defined by a (relative) path, a list of attributes and the name of the template that should be used to render it.
data Page = Page !FilePath ![(String, AttributeValue)] !String
          | OptionalPage FilePath [(String, AttributeValue)] !Bool

-- | Data about the season being published, useful in templates.
data MetaData = MetaData {league :: String,
                          division :: String,
                          season :: String,
                          isAggregated :: Bool,
                          isCollated :: Bool,
                          isArchive :: Bool,
                          isNeutral :: Bool,
                          hasScorers :: Bool,
                          teamLinks :: Map String String,
                          miniLeaguesLink :: Maybe String}

instance ToSElem MetaData where
    toSElem meta = SM $ Map.fromAscList [("division", toSElem $ division meta),
                                         ("hasScorers", toSElem $ hasScorers meta),
                                         ("isAggregated", toSElem $ isAggregated meta),
                                         ("isArchive", toSElem $ isArchive meta),
                                         ("isCollated", toSElem $ isCollated meta),
                                         ("league", toSElem $ league meta),
                                         ("miniLeaguesLink", toSElem $ miniLeaguesLink meta),
                                         ("neutral", toSElem $ isNeutral meta),
                                         ("season", toSElem $ season meta),
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
reduceScorers goalsList = map (pair id (reducedMap!)) players
                          where reducedMap = foldl' addGoalToScorers Map.empty goalsList
                                players = nub $ map scorer goalsList

addGoalToScorers :: Map ByteString String -> Goal -> Map ByteString String
addGoalToScorers players goal = Map.alter (updateScorer goal) (scorer goal) players
                                where updateScorer g Nothing  = Just $ goalTypeString g ++ show (minute g)
                                      updateScorer g (Just d) = Just $ d ++ ", " ++ goalTypeString g ++ show (minute g)

goalTypeString :: Goal -> String
goalTypeString goal = case goalType goal of
                          "p" -> "pen "
                          "o" -> "o.g. "
                          _   -> ""

-- | Generates home, away and overall HTML league tables.
leagueTablePages :: Results -> Map Team Int -> MetaData -> Int -> [Page]
leagueTablePages results adjustments meta sp = [Page "index.html" (tableAttrib byTeam adjustments sp) "table.html",
                                                OptionalPage "hometable.html" (tableAttrib homeOnly Map.empty 0) (not $ isNeutral meta),
                                                OptionalPage "awaytable.html" (tableAttrib awayOnly Map.empty 0) (not $ isNeutral meta),
                                                OptionalPage "firsthalftable.html" (tableAttrib firstHalf Map.empty 0) (hasScorers meta),
                                                OptionalPage "secondhalftable.html" (tableAttrib secondHalf Map.empty 0) (hasScorers meta)]
                                               where tableAttrib rf adj s = [("table", AV $ leagueTable (rf results) adj s), ("tableSelected", AV True), ("metaData", AV meta)]

formTablePages :: Results -> MetaData -> [Page]
formTablePages results meta = [OptionalPage "formtable.html" (formAttrib byTeam 6) (not $ isArchive meta),
                               OptionalPage "homeformtable.html" (formAttrib homeOnly 4) (not $ isArchive meta && not (isNeutral meta)),
                               OptionalPage "awayformtable.html" (formAttrib awayOnly 4) (not $ isArchive meta && not (isNeutral meta))]
                              where formAttrib rf n = [("table", AV $ formTable (rf results) n), ("formSelected", AV True), ("metaData", AV meta)]

-- | Generates current and longest sequences for home, away and all matches.
sequencePages :: Results -> MetaData -> [Page]
sequencePages results meta = [OptionalPage "currentsequences.html" (seqAttribs overallCurrent "currentSequencesSelected") (not $ isArchive meta),
                              Page "longestsequences.html" (seqAttribs overallLongest "longestSequencesSelected") "longestsequences.html",
                              OptionalPage "homecurrentsequences.html" (seqAttribs homeCurrent "currentSequencesSelected") (not $ isArchive meta && not (isNeutral meta)),
                              OptionalPage "homelongestsequences.html" (seqAttribs homeLongest "longestSequencesSelected") (not $ isNeutral meta),
                              OptionalPage "awaycurrentsequences.html" (seqAttribs awayCurrent "currentSequencesSelected") (not $ isArchive meta && not (isNeutral meta)),
                              OptionalPage "awaylongestsequences.html" (seqAttribs awayLongest "longestSequencesSelected") (not $ isNeutral meta)]
                             where (overallCurrent, overallLongest) = getSequenceTables $ byTeam results
                                   (homeCurrent, homeLongest) = getSequenceTables $ homeOnly results
                                   (awayCurrent, awayLongest) = getSequenceTables $ awayOnly results
                                   seqAttribs seqs sel = [("sequences", AV . Map.mapKeys show $ seqs), (sel, AV True), ("metaData", AV meta)]

-- | Generates team aggregates for all matches.
aggregatePages:: Results -> MetaData -> [Page]
aggregatePages results meta = [Page "aggregates.html" attributes "aggregates.html"]
                              where attributes = [("aggregates", AV . Map.mapKeys show . getAggregateTables $ byTeam results), ("aggregatesSelected", AV True), ("metaData", AV meta)]

resultsPages :: Results -> MetaData -> [Page]
resultsPages results meta = [Page "results.html" attributes "results.html"]
                            where homeWinMatches = homeWins $ list results
                                  awayWinMatches = awayWins $ list results
                                  matchCount = length $ list results
                                  homeWinCount = length homeWinMatches
                                  awayWinCount = length awayWinMatches
                                  drawCount = matchCount - homeWinCount - awayWinCount
                                  goalCount = sum . map aggregate $ list results
                                  highAggregates = highestAggregates (list results)
                                  attributes = [("results", AV . Map.toDescList $ byDate results),
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
                                                ("bigWins", AV . biggestWins $ list results),
                                                ("highAggregates", AV highAggregates),
                                                ("resultsSelected", AV True),
                                                ("metaData", AV meta)]

miniLeaguePages :: Results -> [(ByteString, Set Team)] -> Map ByteString Team -> MetaData -> [Page]
miniLeaguePages results miniLeagues aliases meta = map (miniLeaguePage results aliases tabs meta) miniLeagues
                                                   where tabs = map (pair id toHTMLFileName . fst) miniLeagues -- Each tab is a display name and a file name.

miniLeaguePage :: Results -> Map ByteString Team -> [(ByteString, String)] -> MetaData -> (ByteString, Set Team) -> Page
miniLeaguePage results aliases tabs meta (name, teams) = Page (toHTMLFileName name) attributes "minileague.html"
                                                         where selectedTabs = map (\(n, f) -> (n, f, n == name)) tabs -- Add a boolean "selected" flag to each tab.
                                                               attributes = [("table", AV $ miniLeagueTable teams (byTeam results) aliases),
                                                                             ("miniLeaguesSelected", AV True),
                                                                             ("name", AV name),
                                                                             ("bottomTabs", AV selectedTabs),
                                                                             ("metaData", AV meta)]

teamPages :: Results -> Map Team [(Day, Int)] -> MetaData -> [Page]
teamPages results positions meta = map (\t -> teamPage t results (positions ! t) meta) . Map.keys $ byTeam results

-- | Generate the overview page for an individual team.
teamPage :: Team -> Results -> [(Day, Int)] -> MetaData -> Page
teamPage t results positions meta = Page (teamLinks meta ! BS.unpack t) attributes "team.html"
                                    where homeResults = homeOnly results ! t
                                          awayResults = awayOnly results ! t
                                          teamResults = map (convertResult t) $ byTeam results ! t
                                          (goalScorers, ownGoals) = teamGoalScorers teamResults
                                          (goalsForByInterval, goalsAgainstByInterval) = goalsByInterval teamResults
                                          -- Don't include all goal-scorers for aggregated pages because the list could be massive.
                                          goalScorers' = if isAggregated meta then takeAtLeast 10 $ groupBy (equal snd) goalScorers else goalScorers
                                          attributes = [("team", AV t),
                                                        ("results", AV teamResults),
                                                        ("record", AV . getSummary t $ byTeam results ! t),
                                                        ("homeRecord", AV $ getSummary t homeResults),
                                                        ("awayRecord", AV $ getSummary t awayResults),
                                                        ("scorers", AV goalScorers'),
                                                        ("ownGoals", AV $ show ownGoals),
                                                        ("positions", AV positions),
                                                        ("goalsByInterval", AV (goalsForByInterval, goalsAgainstByInterval)),
                                                        ("intervalMaxima", AV (roundUp (maximum goalsForByInterval) 5, roundUp (maximum goalsAgainstByInterval) 5)),
                                                        ("teamCount", AV . Map.size $ teamLinks meta),
                                                        ("metaData", AV meta)]

getSummary :: Team -> [Result] -> (Int, Float, Int, Float, Int, Float)
getSummary t results = (won record,
                        percentage (won record) matches,
                        drawn record,
                        percentage (drawn record) matches,
                        lost record,
                        percentage (lost record) matches)
                       where record = buildRecord t results
                             matches = played record

-- | Generates the top scorers list (only if there are scorers in the data).
goalPages:: Results -> MetaData -> [Page]
goalPages results metaData = [OptionalPage "goals.html" attributes (not $ null players)]
                             where players = topGoalScorers $ list results
                                   attributes = [("scorers", AV players),
                                                 ("penalties", AV . topPenaltyScorers $ list results),
                                                 ("hatTricks", AV . hatTricks $ list results),
                                                 ("goalsSelected", AV True),
                                                 ("metaData", AV metaData)]

-- | Convert a string for use as a filename (converts to lower case and eliminates whitespace).
toHTMLFileName :: ByteString -> String
toHTMLFileName name = BS.unpack $ BS.map toLower (BS.filter (not.isSpace) name) `BS.append` ".html"

-- | Take a list of team names and return mappings for the associated team pages.
mapTeamNames :: [Team] -> Map ByteString Team -> Map String String
mapTeamNames teams aliases = foldl' insert Map.empty teams
                             where insert m t = Map.insert (BS.unpack t) (toHTMLFileName $ Map.findWithDefault t t aliases) m

-- | Generates all stats pages for a given season.
seasonPages :: LeagueData -> MetaData -> [Page]
seasonPages (LeagueData teams res adj miniLeagues sp aliases) meta = leagueTablePages results adj meta sp
                                                                     ++ formTablePages results meta
                                                                     ++ resultsPages results meta
                                                                     ++ sequencePages results meta
                                                                     ++ aggregatePages results meta
                                                                     ++ miniLeaguePages results miniLeagues aliases meta
                                                                     ++ teamPages results positions meta
                                                                     ++ goalPages results meta
                                                                     where results = prepareResults res aliases
                                                                           positions = leaguePositions teams (byDate results) adj sp

-- | Determine which file the "Mini-Leagues" tab should link to (derived from the name of the first mini-league).
--   If there are no mini-leagues then this function returns nothing and the tab should not be shown.
getMiniLeaguesLink :: [(ByteString, Set Team)] -> Maybe String
getMiniLeaguesLink []            = Nothing
getMiniLeaguesLink ((name, _):_) = Just $ toHTMLFileName name

publishLeagues :: STGroup ByteString -> Configuration -> IO ()
publishLeagues templates config = do publishPage templates (outputRoot config) $ Page "selector.json" [("config", AV config)] "selector.json"
                                     mapM_ (publishLeague templates) $ leagues config

publishLeague :: STGroup ByteString -> League -> IO ()
publishLeague templates lg = mapM_ (publishDivision templates (leagueName lg)) $ divisions lg

publishDivision :: STGroup ByteString -> String -> Division -> IO ()
publishDivision templateGroup lgName lgDiv = mapM_ (publishSeason templateGroup lgName (divisionName lgDiv)) $ seasons lgDiv

publishSeason :: STGroup ByteString -> String -> String -> Season -> IO ()
publishSeason templates lgName divName divSeason = do let dataFile = inputFile divSeason
                                                      modified <- isNewer dataFile (outputDir divSeason </> "index.html")
                                                      case modified || aggregated divSeason || collated divSeason of
                                                          False -> print $ "Skipping unchanged file " ++ dataFile 
                                                          True  -> do print $ "Processing " ++ dataFile
                                                                      leagueData@(LeagueData teams _ _ miniLeagues _ aliases) <- parseRLTFile dataFile
                                                                      let links = mapTeamNames (Set.toList teams) aliases
                                                                          meta = MetaData lgName
                                                                                          divName
                                                                                          (seasonName divSeason)
                                                                                          (aggregated divSeason)
                                                                                          (collated divSeason)
                                                                                          (archive divSeason)
                                                                                          (neutral divSeason)
                                                                                          (scorers divSeason)
                                                                                          links
                                                                                          (getMiniLeaguesLink miniLeagues)
                                                                      createDirectoryIfMissing True (outputDir divSeason)
                                                                      mapM_ (publishPage templates (outputDir divSeason)) $ seasonPages leagueData meta

-- | Publish a single page by applying the appropriate HStringTemplate template.
publishPage :: STGroup ByteString -> FilePath -> Page -> IO ()
publishPage group dir (Page name attributes templateName) = case getStringTemplate templateName group of
                                                                 Nothing -> print $ "Could not find template for " ++ templateName
                                                                 Just template  -> BS.writeFile (dir </> name) . render $ setManyAttrib attributes template
publishPage group dir (OptionalPage name attributes True) = publishPage group dir $ Page name attributes name
publishPage _ _ _                                         = return ()
