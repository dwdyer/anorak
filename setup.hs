#!/usr/bin/env runhaskell

import Distribution.PackageDescription(PackageDescription(..))
import Distribution.Simple
import Distribution.Simple.LocalBuildInfo(LocalBuildInfo(..))
import System.Cmd(system)
import System.FilePath((</>))

main :: IO ()
main = defaultMain

