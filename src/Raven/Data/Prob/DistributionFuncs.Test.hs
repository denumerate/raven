module Main(main) where

import Test.HUnit
import System.Exit
import qualified Data.Map as M
import Data.Ratio

import Raven.Data.Prob.DistributionFuncs

main :: IO ()
main = do
  cs@(Counts _ _ errs fails) <- runTestTT allTests
  putStrLn (showCounts cs)
  if errs > 0 || fails > 0
    then exitFailure
    else exitSuccess

allTests :: Test
allTests = TestList $ runtests (uniform M.empty) uniformDataEmpty
           ++ runtests (uniform set1) uniformDataSet1

--run tests with 1 input
runtests :: (Eq b,Show b) => (a -> b) -> [(String,a,b)] -> [Test]
runtests f ls = map (uncurry (~:)) (createtest f ls)

--create tests with 1 input
createtest :: (Eq b,Show b) => (a -> b) -> [(String,a,b)] -> [(String,Test)]
createtest f = map (\(s,x,y) -> (s,TestCase $ y @=? f x))

--Test Values:
set1 = M.fromList $ zip [0..4] $ take 5 $ repeat $ 1%5

uniformDataEmpty :: [(String,Rational,Rational)]
uniformDataEmpty =
  [ ("uniform: empty",8,0)
  , ("uniform: empty 1",-4,0)
  ]

uniformDataSet1 :: [(String,Rational,Rational)]
uniformDataSet1 =
  [ ("uniform: zero",-4,0)
  , ("uniform: zero 1",5%4,0)
  , ("uniform: norm",2%3,1%5)
  , ("uniform: norm",4%5,1%5)
  ]
