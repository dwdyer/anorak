{-# LANGUAGE DeriveDataTypeable #-}

-- | Configuration loader for the Anorak system.
module Anorak.Config (Configuration(..), ConfigurationException, Division(..), League(..), readConfig, Season(..)) where

import Control.Exception(Exception, throw)
import Data.Typeable(Typeable)
import System.FilePath(combine, isRelative, takeDirectory)
import Text.XML.Light(Attr(..), Content(..), Element(..), findAttr, findChildren, parseXMLDoc, QName(..))

data Configuration = Configuration {outputRoot :: FilePath, leagues :: [League]}
-- | Configuration consists of a list of leagues, each with a name and one or more divisions.
data League = League {leagueName :: String, divisions :: [Division]}
-- | A division has a name and one or more seasons.
data Division = Division {divisionName :: String, seasons :: [Season]}
-- | A season has a name and is defined by the contents of a data file.
data Season = Season {seasonName :: String,  -- ^ The name of the season (e.g. "1997/98").
                      inputFile :: FilePath, -- ^ Path to the season's data file.
                      outputDir :: FilePath  -- ^ The directory to write the generated files to.
                     }

-- | A ConfigurationException is thrown when there is a problem processing the XML configuration file.
data ConfigurationException = ConfigurationException String deriving (Typeable, Show)
instance Exception ConfigurationException

-- | Load the specified XML file and return the league configurations that it contains.
readConfig :: FilePath -> IO Configuration
readConfig file = do xml <- readFile file
                     let document = parseXMLDoc xml
                     case document of
                         Nothing      -> throw (ConfigurationException "Invalid XML configuration.")
                         Just element -> return $ convertXMLDocumentToConfig file element

convertXMLDocumentToConfig :: FilePath -> Element -> Configuration
convertXMLDocumentToConfig configFile element = Configuration outputDir leagues
                                                where outputDir = mapPath (getAttributeValue element "output") (takeDirectory configFile)
                                                      inputDir = takeDirectory configFile
                                                      leagues = map (processLeagueTag inputDir outputDir) $ findChildren (QName "league" Nothing Nothing) element

processLeagueTag :: FilePath -> FilePath -> Element -> League
processLeagueTag baseDir outputDir tag = League (getAttributeValue tag "name") (map (processDivisionTag baseDir outputDir) $ findChildren (QName "division" Nothing Nothing) tag)

processDivisionTag :: FilePath -> FilePath -> Element -> Division
processDivisionTag baseDir outputDir tag = Division (getAttributeValue tag "name") (map (processSeasonTag baseDir outputDir) $ findChildren (QName "season" Nothing Nothing) tag)

processSeasonTag :: FilePath -> FilePath -> Element -> Season
processSeasonTag baseDir outputDir tag = Season (getAttributeValue tag "name")
                                                (mapPath (getAttributeValue tag "input") baseDir)
                                                (mapPath (getAttributeValue tag "output") outputDir)

getAttributeValue :: Element -> String -> String
getAttributeValue element name = case findAttr (QName name Nothing Nothing) element of
                                     Nothing    -> throw $ ConfigurationException $ "Missing attribute: " ++ name
                                     Just value -> value

-- | If the XML includes a relative path (relative to the config file itself), map it to an absolute path.
mapPath :: FilePath -> FilePath -> FilePath
mapPath path base
    | isRelative(path) = combine base path
    | otherwise        = path
