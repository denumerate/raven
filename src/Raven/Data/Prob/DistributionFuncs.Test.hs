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
allTests = TestList $ runtests (uniformDistribution M.empty) uniformDistributionDataEmpty
  ++ runtests (uniformDistribution set1) uniformDistributionDataSet1
  ++ runtests (intUniformDistribution M.empty) intUniformDistributionDataEmpty
  ++ runtests (intUniformDistribution set1') intUniformDistributionDataSet1
  ++ runtests (uniformDensity M.empty) uniformDensityDataEmtpy
  ++ runtests (uniformDensity set1) uniformDensityDataSet1
  ++ runtests (intUniformDensity M.empty) intUniformDensityDataEmtpy
  ++ runtests (intUniformDensity set1') intUniformDensityDataSet1

--run tests with 1 input
runtests :: (Eq b,Show b) => (a -> b) -> [(String,a,b)] -> [Test]
runtests f ls = map (uncurry (~:)) (createtest f ls)

--create tests with 1 input
createtest :: (Eq b,Show b) => (a -> b) -> [(String,a,b)] -> [(String,Test)]
createtest f = map (\(s,x,y) -> (s,TestCase $ y @=? f x))

--Test Values:
set1 = M.fromList $ zip [0..4] $ take 5 $ repeat $ 1%5
set1' = M.fromList $ zip [0..4] $ take 5 $ repeat $ 1%5

uniformDistributionDataEmpty :: [(String,Rational,Rational)]
uniformDistributionDataEmpty =
  [ ("uniformDistribution: empty", 1%5, 0)
  , ("uniformDistribution: empty 1", -4%7, 0)
  ]

uniformDistributionDataSet1 :: [(String,Rational,Rational)]
uniformDistributionDataSet1 =
  [ ("uniformDistribution: zero",0,0)
  , ("uniformDistribution: one",4,1)
  , ("uniformDistribution: norm",1%2,1%8)
  , ("uniformDistribution: norm 1",3,3%4)
  , ("uniformDistribution: norm 2",7%3,7%12)
  ]

intUniformDistributionDataEmpty :: [(String,Int,Ratio Int)]
intUniformDistributionDataEmpty =
  [ ("intUniformDistribution: empty", 1, 0)
  , ("intUniformDistribution: empty 1", -4, 0)
  ]


intUniformDistributionDataSet1 :: [(String,Int,Ratio Int)]
intUniformDistributionDataSet1 =
  [ ("intUniformDistribution: zero",0,0)
  , ("intUniformDistribution: one",4,1)
  , ("intUniformDistribution: norm",1,1%4)
  , ("intUniformDistribution: norm 1",2,1%2)
  , ("intUniformDistribution: norm 2",3,3%4)
  ]

uniformDensityDataEmtpy :: [(String,Rational,Rational)]
uniformDensityDataEmtpy =
  [ ("uniformDensity: empty", 6%11, 0)
  , ("uniformDensity: empty 1", -8%3,0)
  ]

uniformDensityDataSet1 :: [(String,Rational,Rational)]
uniformDensityDataSet1 =
  [ ("uniformDensity: zero",-1%3,0)
  , ("uniformDensity: zero 1",9%2,0)
  , ("uniformDensity: norm",3%4,1%4)
  , ("uniformDensity: norm 1",7%3,1%4)
  ]

intUniformDensityDataEmtpy :: [(String,Int,Ratio Int)]
intUniformDensityDataEmtpy =
  [ ("intUniformDensity: empty", 6, 0)
  , ("intUniformDensity: empty 1", -8,0)
  ]

intUniformDensityDataSet1 :: [(String,Int,Ratio Int)]
intUniformDensityDataSet1 =
  [ ("intUniformDensity: zero",-1,0)
  , ("intUniformDensity: zero 1",5,0)
  , ("intUniformDensity: norm",3,1%4)
  , ("intUniformDensity: norm 1",2,1%4)
  ]
