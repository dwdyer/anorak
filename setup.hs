#!/usr/bin/env runhaskell

import Distribution.PackageDescription(PackageDescription(..))
import Distribution.Simple
import Distribution.Simple.LocalBuildInfo(LocalBuildInfo(..))
import System.Cmd(system)
import System.FilePath((</>))

main :: IO ()
main = defaultMainWithHooks hooks
       where hooks = simpleUserHooks {runTests = runTests'}

runTests' :: Args -> Bool -> PackageDescription -> LocalBuildInfo -> IO ()
runTests' _ _ _ lbi = system testprog >> return ()
                      where testprog = (buildDir lbi) </> "test" </> "test"

