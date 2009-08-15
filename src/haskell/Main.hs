module Main where

import Anorak.Config
import Anorak.Publisher
import System(getArgs)
import Text.StringTemplate(directoryGroup, STGroup)

-- | Expects three arguments - the path to the XML config file, the path to the templates directory and the path to the
--   output directory.
main :: IO ()
main = do [configFile, templateDir, outputDir] <- getArgs
          leagues <- readConfig configFile
          let seasonsData = concat $ map seasons $ concat $ map divisions leagues 
          group <- directoryGroup templateDir :: IO (STGroup String)
          copyResources templateDir outputDir
          mapM_ (processDataFile group) seasonsData

