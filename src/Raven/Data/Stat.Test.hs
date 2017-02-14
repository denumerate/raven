module Main(main) where

import Test.HUnit
import System.Exit
import Data.Ratio
import Data.Map (Map)
import qualified Data.Map as M

import Raven.Data.Stat

main :: IO ()
main = do
  cs@(Counts _ _ errs fails) <- runTestTT allTests
  putStrLn (showCounts cs)
  if errs > 0 || fails > 0
    then exitFailure
    else exitSuccess

allTests :: Test
allTests = TestList $ runtests mean meanData
           ++ runtests intMean intMeanData
           ++ runtests ratioMean ratioMeanData
           ++ runtests median medianData
           ++ runtests intMedian intMedianData
           ++ runtests ratioMedian ratioMedianData
           ++ runtests countInstances countInstancesData
           ++ runtests mode modeData

--run tests with 1 input
runtests :: (Eq b,Show b) => (a -> b) -> [(String,a,b)] -> [Test]
runtests f ls = map (uncurry (~:)) (createtest f ls)

--create tests with 1 input
createtest :: (Eq b,Show b) => (a -> b) -> [(String,a,b)] -> [(String,Test)]
createtest f = map (\(s,x,y) -> (s,TestCase $ y @=? f x))

meanData :: [(String,[Double],Double)]
meanData =
  [ ("mean: empty",[],0)
  , ("mean: single",[1],1)
  , ("mean: norm",[1,2,3],2)
  , ("mean: norm 1",[1,3,4],8/3)
  , ("mean: norm 2",[-1,2.5],1.5/2)
  ]

intMeanData :: [(String,[Int],Ratio Int)]
intMeanData =
  [ ("intMean: empty",[],0)
  , ("intMean: single",[2],2)
  , ("intMean: norm",[1,2,3],2)
  , ("intMean: norm 1",[1,2],3%2)
  , ("intMean: norm 2",[1,3,4],8%3)
  , ("intMean: norm 3",[-1,2,4],5%3)
  ]

ratioMeanData :: [(String,[Ratio Int],Ratio Int)]
ratioMeanData =
  [ ("ratioMean: empty",[],0)
  , ("ratioMean: single",[1],1)
  , ("ratioMean: norm",[1,2],3%2)
  , ("ratioMean: norm 1",[1%2,3%2],1)
  , ("ratioMean: norm 2",[-1%4,3%4],1%4)
  ]

medianData :: [(String,[Double],Double)]
medianData =
  [ ("median: empty",[],0)
  , ("median: single",[1],1)
  , ("median: double",[2,1],3/2)
  , ("median: norm",[3,1,2],2)
  , ("median: norm 1",[1.5,-0.5],0.5)
  , ("median: norm 2",[4,2,6,8,9,1,5,3,7],5)
  , ("median: norm 3",[5,10,1,4,3,7,6,8,2,9],5.5)
  , ("median: norm 4",[9,4,6],6)
  ]

intMedianData :: [(String,[Int],Ratio Int)]
intMedianData =
  [ ("intMedian: empty",[],0)
  , ("intMedian: single",[4],4)
  , ("intMedian: double",[2,3],5%2)
  , ("intMedian: norm",[3,1,2],2)
  , ("intMedian: norm 1",[-1,5,0],0)
  , ("intMedian: norm 2",[5,10,1,4,3,7,6,8,2,9],11%2)
  , ("intMedian: norm 3",[4,2,6,8,9,1,5,3,7],5)
  , ("intMedian: norm 4",[3,-1,6,7],9%2)
  ]

ratioMedianData :: [(String,[Ratio Int],Ratio Int)]
ratioMedianData =
  [ ("ratioMedian: empty",[],0)
  , ("ratioMedian: single",[-1],-1)
  , ("ratioMedian: double",[4%3,2%3],1)
  , ("ratioMedian: norm",[4%3,-5%2,7],4%3)
  , ("ratioMedian: norm 1",[2%3,1%2,-6,5],7%12)
  ]

countInstancesData :: [(String,[Int],Map Int Int)]
countInstancesData =
  [ ("countInstances: empty",[],M.empty)
  , ("countInstances: single",[2],M.fromList [(2,1)])
  , ("countInstances: no repeats",[1..4],
    M.fromList [(1,1),(2,1),(3,1),(4,1)])
  , ("countInstances: all repeat",[2,2,2],M.fromList [(2,3)])
  , ("countInstances: norm",[1,2,3,2],
    M.fromList [(1,1),(2,2),(3,1)])
  ]

modeData :: [(String,[Int],[(Int,Int)])]
modeData =
  [ ("mode: empty",[],[])
  , ("mode: single",[5],[(5,1)])
  , ("mode: norm",[1,2,3],[(3,1),(2,1),(1,1)])
  , ("mode: norm 1",[1,2,3,2],[(2,2)])
  ]
