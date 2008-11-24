-- | HTML publishing module for the Anorak system.
module Anorak.Publisher where

import Anorak.Core
import Anorak.Types
import Anorak.RLTParser
import Data.Map(Map)
import qualified Data.Map as Map(empty, fromAscList)
import List(isPrefixOf, isSuffixOf)
import Monad(filterM)
import System(getArgs)
import System.Directory(createDirectoryIfMissing, copyFile, doesFileExist, getDirectoryContents)
import System.FilePath(combine, replaceDirectory, takeFileName)
import Text.ParserCombinators.Parsec(ParseError)
import Text.StringTemplate(directoryGroup, getStringTemplate, setAttribute, STGroup, StringTemplate, stShowsToSE, toString)
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

-- | Renders a league table as an HTML page using the specified template.
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

-- | Expects three arguments - the path to the RLT data file, the path to the templates directory and the path to the
--   output directory.
main :: IO ()
main = do dataFile:templateDir:outputDir:_ <- getArgs
          (teams, results, adjustments) <- parseRLTFile dataFile
          group <- directoryGroup templateDir :: IO (STGroup String)
          createDirectoryIfMissing True outputDir
          copyResources templateDir outputDir          
          case getStringTemplate "leaguetable.html" group of
              Nothing       -> print "Could not find league table template."
              Just template -> writeFile (combine outputDir "leaguetable.html") $ htmlLeagueTable table template
                               where table = leagueTable (resultsByTeam results Map.empty) adjustments

