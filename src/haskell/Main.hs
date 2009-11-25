module Main where

import Anorak.Config(readConfig, outputRoot)
import Anorak.Publisher
import System(getArgs)
import Text.StringTemplate(directoryGroup, STGroup)

-- | Expects two arguments - the path to the XML config file and the path to the templates directory.
main :: IO ()
main = do [configFile, templateDir] <- getArgs
          config <- readConfig configFile
          group <- directoryGroup templateDir :: IO (STGroup String)
          copyResources templateDir (outputRoot config)
          publishLeagues group config

