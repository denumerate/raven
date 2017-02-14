module Main(main) where

import Test.HUnit
import System.Exit
import Data.Vector (Vector)
import qualified Data.Vector as V

import Raven.Data.BasicEntry
import Raven.Data.Entry

main :: IO ()
main = do
  cs@(Counts _ _ errs fails) <- runTestTT allTests
  putStrLn (showCounts cs)
  if errs > 0 || fails > 0
    then exitFailure
    else exitSuccess

allTests :: Test
allTests = TestList $ runtests getEntries getEntriesData

--run tests with 1 input
runtests :: (Eq b,Show b) => (a -> b) -> [(String,a,b)] -> [Test]
runtests f ls = map (uncurry (~:)) (createtest f ls)

--create tests with 1 input
createtest :: (Eq b,Show b) => (a -> b) -> [(String,a,b)] -> [(String,Test)]
createtest f = map (\(s,x,y) -> (s,TestCase $ y @=? f x))

readVector :: (Entry a) => [String] -> Vector a
readVector = V.fromList . map readEntry

--Test values:
navector = ["NA","NA","NA"]

vector1 = ["1","2","3"]

vector2 = ["1","NA","2"]

vector3 = ["1","2.0","2","NA"]

vector4 = ["test","NA","4","3/4"]

getEntriesData :: [(String,Vector BasicEntry,[Int])]
getEntriesData =
  [ ("getEntries: empty",V.empty,[])
  , ("getEntries: all NA's",readVector navector,[])
  , ("getEntries: norm",readVector vector1,[1..3])
  , ("getEntries: some NA'a",readVector vector2,[1,2])
  , ("getEntries: mixed",readVector vector3,[1,2])
  , ("getEntries: mised 1",readVector vector4,[4])
  ]
