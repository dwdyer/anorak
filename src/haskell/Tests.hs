import qualified Util.Tests as Util(tests)
import Test.QuickCheck.Batch(defOpt, runTests)
import qualified Anorak.Tests as Anorak(tests)

main :: IO ()
main = runTests "tests" defOpt $ Util.tests ++ Anorak.tests
