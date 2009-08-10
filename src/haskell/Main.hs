module Main where

import Anorak.Publisher
import System(getArgs)
import Text.StringTemplate(directoryGroup, STGroup)

-- | Expects three arguments - the path to the RLT data files, the path to the templates directory and the path to the
--   output directory.
main :: IO ()
main = do dataDir:templateDir:outputDir:_ <- getArgs
          dataFiles <- findDataFiles dataDir
          group <- directoryGroup templateDir :: IO (STGroup String)
          copyResources templateDir outputDir
          mapM_ (processDataFile dataDir outputDir group) dataFiles
