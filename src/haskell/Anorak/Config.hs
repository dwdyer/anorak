{-# LANGUAGE DeriveDataTypeable #-}

-- | Configuration loader for the Anorak system.
module Anorak.Config (Configuration(..), ConfigurationException, Division(..), League(..), readConfig, Season(..)) where

import Anorak.Utils(makeAbsolute)
import Control.Exception(Exception, throw)
import Data.Data(Data)
import Data.Typeable(Typeable)
import System.FilePath(takeDirectory)
import Text.XML.Light(Element(..), findAttr, findChildren, parseXMLDoc, QName(..))

data Configuration = Configuration {outputRoot :: FilePath,
                                    templateDir :: FilePath,
                                    leagues :: [League]}
    deriving (Data, Typeable)

-- | Configuration consists of a list of leagues, each with a name and one or more divisions.
data League = League {leagueName :: String, divisions :: [Division]}
    deriving (Data, Typeable)

-- | A division has a name and one or more seasons.
data Division = Division {divisionName :: String, seasons :: [Season]}
    deriving (Data, Typeable)

-- | A season has a name and is defined by the contents of a data file.
data Season = Season {seasonName :: String,     -- ^ The name of the season (e.g. "1997/98").
                      inputFile :: FilePath,    -- ^ Path to the season's data file.
                      outputDir :: FilePath,    -- ^ The directory to write the generated files to.
                      relativeLink :: FilePath, -- ^ Link relative to the web root.
                      aggregated :: Bool,       -- ^ Whether this is an aggregate of multiple seasons from the same division.
                      collated :: Bool}         -- ^ Whether this is the combination of multiple divisions from the same season.
    deriving (Data, Typeable)

-- | A ConfigurationException is thrown when there is a problem processing the XML configuration file.
data ConfigurationException = ConfigurationException String
    deriving (Typeable, Show)
instance Exception ConfigurationException

-- | Load the specified XML file and return the league configurations that it contains.
readConfig :: FilePath -> IO Configuration
readConfig file = do xml <- readFile file
                     let document = parseXMLDoc xml
                     case document of
                         Nothing      -> throw (ConfigurationException "Invalid XML configuration.")
                         Just element -> return $ convertXMLDocumentToConfig file element

convertXMLDocumentToConfig :: FilePath -> Element -> Configuration
convertXMLDocumentToConfig configFile element = Configuration outputDir templateDir leagues
                                                where configDir = takeDirectory configFile
                                                      outputDir = makeAbsolute (getAttributeValue element "output") configDir
                                                      templateDir = makeAbsolute (getAttributeValue element "templates") configDir
                                                      inputDir = takeDirectory configFile
                                                      leagues = map (processLeagueTag inputDir outputDir) $ findChildren (xmlName "league") element

processLeagueTag :: FilePath -> FilePath -> Element -> League
processLeagueTag baseDir outputDir tag = League (getAttributeValue tag "name") (map (processDivisionTag baseDir outputDir) $ findChildren (xmlName "division") tag)

processDivisionTag :: FilePath -> FilePath -> Element -> Division
processDivisionTag baseDir outputDir tag = Division (getAttributeValue tag "name") (map (processSeasonTag baseDir outputDir) $ findChildren (xmlName "season") tag)

processSeasonTag :: FilePath -> FilePath -> Element -> Season
processSeasonTag baseDir outputDir tag = Season (getAttributeValue tag "name")
                                                (makeAbsolute (getAttributeValue tag "input") baseDir)
                                                (makeAbsolute seasonDir outputDir)
                                                ("../../../" ++ seasonDir ++ "/index.html")
                                                (getBooleanAttribute tag "aggregated")
                                                (getBooleanAttribute tag "collated")
                                         where seasonDir = getAttributeValue tag "output"

-- | Simplifies the reading of XML attributes by assuming that the attribute is present.  Throws an exception if it is not.
getAttributeValue :: Element -> String -> String
getAttributeValue element name = case findAttr (xmlName name) element of
                                     Nothing    -> throw . ConfigurationException $ "Missing attribute: " ++ name
                                     Just value -> value

-- | Look up a named attribute.  If it is present and the value is "true", return True, else if it's not present or has some other value return False.
getBooleanAttribute :: Element -> String -> Bool
getBooleanAttribute element name = case findAttr (xmlName name) element of
                                       Nothing    -> False
                                       Just value -> value == "true"

-- | Simple QNames.
xmlName :: String -> QName
xmlName name = QName name Nothing Nothing
