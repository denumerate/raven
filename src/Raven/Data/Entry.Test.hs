module Main(main) where

import Test.HUnit
import System.Exit
import Data.Vector (Vector)
import qualified Data.Vector as V

import Raven.Data.Entry

main :: IO ()
main = do
  cs@(Counts _ _ errs fails) <- runTestTT allTests
  putStrLn (showCounts cs)
  if errs > 0 || fails > 0
    then exitFailure
    else exitSuccess

allTests :: Test
allTests = TestList $ runtests getEntryVector getEntryVectorData

--run tests with 1 input
runtests :: (Eq b,Show b) => (a -> b) -> [(String,a,b)] -> [Test]
runtests f ls = map (uncurry (~:)) (createtest f ls)

--create tests with 1 input
createtest :: (Eq b,Show b) => (a -> b) -> [(String,a,b)] -> [(String,Test)]
createtest f = map (\(s,x,y) -> (s,TestCase $ y @=? f x))

--Test values:

navector = V.fromList $ map readEntry ["NA","NA","NA"]

getEntryVectorData :: [(String,Vector BasicEntry,Vector Int)]
getEntryVectorData =
  [ ("getEntryVector: empty",V.empty,V.empty)
  , ("getEntryVector: all NA's",navector,V.empty)
  ]
