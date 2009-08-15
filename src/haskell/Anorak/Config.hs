-- | Configuration loader for the Anorak system.
module Anorak.Config (Division(..), League(..), readConfig, Season(..)) where

import System.FilePath(combine, isRelative, takeDirectory)
import Text.XML.Light(Attr(..), Content(..), Element(..), findAttr, findChildren, parseXMLDoc, QName(..))

-- | Configuration consists of a list of leagues, each with a name and one or more divisions.
data League = League {leagueName :: String, divisions :: [Division]}
-- | A division has a name and one or more seasons.
data Division = Division {divisionName :: String, seasons :: [Season]}
-- | A season has a name and is defined by the contents of a data file.
data Season = Season {seasonName :: String,  -- ^ The name of the season (e.g. "1997/98").
                      inputFile :: FilePath, -- ^ Path to the season's data file.
                      outputDir :: FilePath, -- ^ The directory to write the generated files to.
                      current :: Bool        -- ^ Is this a current season?  If it is we generate form tables and current sequences.
                     }

-- | Load the specified XML file and return the league configurations that it contains.
readConfig :: FilePath -> IO [League]
readConfig file = do xml <- readFile file
                     let document = parseXMLDoc xml
                     case document of
                         Nothing      -> return []
                         Just element -> return $ convertXMLDocumentToConfig file element

convertXMLDocumentToConfig :: FilePath -> Element -> [League]
convertXMLDocumentToConfig configFile element = map (processLeagueTag configFile) $ findChildren (QName "league" Nothing Nothing) element

processLeagueTag :: FilePath -> Element -> League
processLeagueTag baseFile tag = League (getAttributeValue tag "name") (map (processDivisionTag baseFile) $ findChildren (QName "division" Nothing Nothing) tag)

processDivisionTag :: FilePath -> Element -> Division
processDivisionTag baseFile tag = Division (getAttributeValue tag "name") (map (processSeasonTag baseFile) $ findChildren (QName "season" Nothing Nothing) tag)

processSeasonTag :: FilePath -> Element -> Season
processSeasonTag baseFile tag = Season (getAttributeValue tag "name")
                                       (mapPath (getAttributeValue tag "input") baseFile)
                                       (mapPath (getAttributeValue tag "output") baseFile)
                                       True

getAttributeValue :: Element -> String -> String
getAttributeValue element name = case findAttr (QName name Nothing Nothing) element of
                                     Nothing    -> "???"
                                     Just value -> value

-- | If the XML includes a relative path (relative to the config file itself), map it to an absolute path.
mapPath :: FilePath -> FilePath -> FilePath
mapPath path base
    | isRelative(path) = combine (takeDirectory base) path
    | otherwise        = path
