#!/usr/bin/env runhaskell

import Distribution.PackageDescription(PackageDescription(..))
import Distribution.Simple
import Distribution.Simple.LocalBuildInfo(LocalBuildInfo(..))
import System.FilePath((</>))

main :: IO ()
main = defaultMain

