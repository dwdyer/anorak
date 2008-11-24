-- | Main module for the Anorak system.
module Anorak (leagueTable) where

import Anorak.Types
import Anorak.RLTParser
import Data.Map(Map)
import qualified Data.Map as Map
import List(isPrefixOf, isSuffixOf, sort)
import Monad(filterM)
import System(getArgs)
import System.Directory(copyFile, doesFileExist, getDirectoryContents)
import System.FilePath(combine, replaceDirectory, takeFileName)
import Text.ParserCombinators.Parsec(ParseError)
import Text.StringTemplate
import Text.StringTemplate.Classes

instance ToSElem LeagueRecord where
    toSElem record = SM $ Map.fromList [("team", toSElem $ team record),
                                        ("played", toSElem $ played record),
                                        ("won", toSElem $ won record),
                                        ("drawn", toSElem $ drawn record),
                                        ("lost", toSElem $ lost record),
                                        ("for", toSElem $ for record),
                                        ("against", toSElem $ against record),
                                        ("goalDiff", toSElem $ goalDiff record),
                                        ("points", toSElem $ points record)]

-- | Builds a LeagueRecord for the specified team, including all of the results (from those provided) in which that
--   team was involved.
buildRecord :: Team -> [Result] -> LeagueRecord
buildRecord team results = foldl (addResultToRecord team) (LeagueRecord team 0 0 0 0 0 0) results

-- | Adds a single match result to a particular team's league record.  If the specified team was not involved in that
--   match, the match is ignored.
addResultToRecord :: Team -> LeagueRecord -> Result -> LeagueRecord
addResultToRecord team record result 
    | team == (homeTeam result) = addScoreToRecord record (homeGoals result) (awayGoals result)
    | team == (awayTeam result) = addScoreToRecord record (awayGoals result) (homeGoals result)
    | otherwise                 = record

addScoreToRecord :: LeagueRecord -> Int -> Int -> LeagueRecord
addScoreToRecord (LeagueRecord team won drawn lost for against adjustment) scored conceded
    | scored > conceded  = (LeagueRecord team (won + 1) drawn lost (for + scored) (against + conceded) adjustment)
    | scored == conceded = (LeagueRecord team won (drawn + 1) lost (for + scored) (against + conceded) adjustment)
    | otherwise          = (LeagueRecord team won drawn (lost + 1) (for + scored) (against + conceded) adjustment)

-- | Convert a flat list of results into a mapping from team to list of results that that team was involved in.
resultsByTeam :: [Result] -> Map Team [Result] -> Map Team [Result]
resultsByTeam [] map          = map
resultsByTeam (result:rs) map = resultsByTeam rs (Map.insertWith (++) (awayTeam result) [result] map')
                                where map' = Map.insertWith (++) (homeTeam result) [result] map

-- | Produces a standard league table with teams ordered in descending order of points.  Takes a map of teams to
--   results and a map of points adjustments and returns a sorted list of league records.
leagueTable :: Map Team [Result] -> Map Team Int -> [LeagueRecord]
leagueTable teamResults adjustments  = sort $ map (adjust adjustments) table
                                       where table = Map.elems $ Map.mapWithKey (buildRecord) teamResults

-- | Looks up the points adjustment for a team (if any) and applies it to their league record.
adjust :: Map Team Int -> LeagueRecord -> LeagueRecord
adjust adjustments (LeagueRecord t w d l f a adj) = (LeagueRecord t w d l f a (adj + Map.findWithDefault 0 t adjustments))

htmlLeagueTable :: [LeagueRecord] -> StringTemplate String -> String
htmlLeagueTable table template = toString $ setAttribute "table" table template

-- | Copies all non-template files from the source directory to the target directory.  Used for making sure that CSS
--   files and images (if any) are deployed with the generated HTML.
copyResources :: FilePath -> FilePath -> IO ()
copyResources from to = do files <- getDirectoryContents from
                           let absoluteFiles = map (combine from) files -- Convert file names into absolute paths.
                           resources <- filterM (isResourceFile) absoluteFiles
                           print resources
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

main :: IO ()
main = do dataFile:templateDir:outputDir:_ <- getArgs
          (teams, results, adjustments) <- parseRLTFile dataFile
          group <- directoryGroup templateDir :: IO (STGroup String)
          case getStringTemplate "leaguetable.html" group of
              Nothing       -> print "Could not find league table template."
              Just template -> writeFile (outputDir ++ "/leaguetable.html") $ htmlLeagueTable table template
                               where table = leagueTable (resultsByTeam results Map.empty) adjustments
          copyResources templateDir outputDir          

