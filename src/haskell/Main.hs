module Main where

import Anorak.Config(Configuration(..), readConfig)
import Anorak.FeatureExtractor(generateFeatures)
import Anorak.Publisher
import Data.ByteString(ByteString)
import System(getArgs)
import Text.StringTemplate(directoryGroup, STGroup)

-- | First argument is the command.  Any subsequent arguments are command-specific.
main :: IO ()
main = do (command:parameters) <- getArgs
          case command of
            "publish"  -> publish $ head parameters
            "features" -> generateFeatures $ head parameters
            _          -> print $ "Unknown option: " ++ command

-- | Publish HTML pages for the configured data files, using the templates in the specified directory.
publish :: FilePath -> IO ()
publish configFile = do config <- readConfig configFile
                        let templates = templateDir config
                        group <- directoryGroup templates :: IO (STGroup ByteString)
                        copyResources templates $ outputRoot config
                        publishLeagues group config
