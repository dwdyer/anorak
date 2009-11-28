module Main where

import Anorak.Config(Configuration, outputRoot, readConfig)
import Anorak.FeatureExtractor(generateFeatures)
import Anorak.Publisher
import System(getArgs)
import Text.StringTemplate(directoryGroup, STGroup)

-- | First argument is the command.  Any subsequent arguments are command-specific.
main :: IO ()
main = do (command:parameters) <- getArgs
          case command of
            "publish"  -> publish (head parameters) (parameters !! 1)
            "features" -> generateFeatures $ head parameters
            _          -> print $ "Unknown option: " ++ command

-- | Publish HTML pages for the configured data files, using the templates in the specified directory.
publish :: FilePath -> FilePath -> IO ()
publish configFile templateDir = do config <- readConfig configFile
                                    group <- directoryGroup templateDir :: IO (STGroup String)
                                    copyResources templateDir (outputRoot config)
                                    publishLeagues group config
