{-# LANGUAGE OverloadedStrings #-}
module Main(main) where

import Test.HUnit
import System.Exit
import qualified Data.Map as M
import Data.Ratio
import Data.Text (Text)

import Raven.Data.Prob

main :: IO ()
main = do
  cs@(Counts _ _ errs fails) <- runTestTT allTests
  putStrLn (showCounts cs)
  if errs > 0 || fails > 0
    then exitFailure
    else exitSuccess

allTests :: Test
allTests = TestList $ runtests2 subSetProb subSetProbData
           ++ runtests combineProbSets combineProbSetsData
           ++ runtests getAllEvents getAllEventsData
           ++ runtests3 conditionalProb conditionalProbData
           ++ runtests3 bayes bayesData
           ++ runtests (densityFunc M.empty) densityFuncDataEmpty
           ++ runtests (densityFunc set1) densityFuncDataSet1
           ++ runtests (densityFunc set2) densityFuncDataSet2

--run tests with 1 input
runtests :: (Eq b,Show b) => (a -> b) -> [(String,a,b)] -> [Test]
runtests f ls = map (uncurry (~:)) (createtest f ls)

--create tests with 1 input
createtest :: (Eq b,Show b) => (a -> b) -> [(String,a,b)] -> [(String,Test)]
createtest f = map (\(s,x,y) -> (s,TestCase $ y @=? f x))

--run tests with 2 inputs
runtests2 :: (Eq c,Show c) => (a -> b -> c) -> [(String,a,b,c)] -> [Test]
runtests2 f ls = map (uncurry (~:)) (createtest2 f ls)

--create tests with 2 inputs
createtest2 :: (Eq c,Show c) => (a -> b -> c) -> [(String,a,b,c)]
  -> [(String,Test)]
createtest2 f = map (\(s,x,y,z) -> (s,TestCase $ z @=? f x y))

--run tests with 3 inputs
runtests3 :: (Eq d,Show d) => (a -> b -> c -> d) -> [(String,a,b,c,d)] -> [Test]
runtests3 f ls = map (uncurry (~:)) (createtest3 f ls)

--create tests with 3 inputs
createtest3 :: (Eq d,Show d) => (a -> b -> c -> d) -> [(String,a,b,c,d)]
  -> [(String,Test)]
createtest3 f = map (\(s,w,x,y,z) -> (s,TestCase $ z @=? f w x y))

--Test values:
set1 = M.fromList [(1,2%3),(2,1%3)]

set2 = M.fromList [(1,1%4),(2,1%8),(3,3%8),(4,1%4)]

set3 = M.fromList [("1",2%3),("2",1%3)]

set4 = M.fromList [("1",1%4),("2",1%8),("3",3%8),("4",1%4)]

set5 = M.fromList [("1",2%5),("2",3%10),("3",1%5),("4",1%10)]

set6 = M.fromList [("1;1",1%6),("1;2",1%12),("1;3",1%4),("1;4",1%6),("2;1",1%12)
                  ,("2;2",1%24),("2;3",1%8),("2;4",1%12)]

set7 = M.fromList [("1;1",4%15),("1;2",1%5),("1;3",2%15),("1;4",1%15)
                  ,("2;1",2%15),("2;2",1%10),("2;3",1%15),("2;4",1%30)]

subSetProbData :: [(String,ProbSet Int Rational,[Int],Rational)]
subSetProbData =
  [ ("subSetProb: empty",M.empty,[],0)
  , ("subSetProb: empty 1",M.empty,[12,3],0)
  , ("subSetProb: empty 2",set1,[],0)
  , ("subSetProb: missing event",set1,[7],0)
  , ("subSetProb: missing event 1",set1,[2,-1,1],0)
  , ("subSetProb: all",set1,[1,2],1)
  , ("subSetProb: all 1",set2,[1..4],1)
  , ("subSetProb: norm",set1,[2],1%3)
  , ("subSetProb: norm 1",set2,[3],3%8)
  , ("subSetProb: norm 2",set2,[1,3,4],7%8)
  , ("subSetProb: norm 3",set2,[2,4],3%8)
  ]

combineProbSetsData :: [(String,[ProbSet Text Rational],ProbSet Text Rational)]
combineProbSetsData =
  [ ("combineProbSets: empty",[],M.empty)
  , ("combineProbSets: single",[set3],set3)
  , ("combineProbSets: norm",[set3,set4],set6)
  , ("combineProbSets: norm 1",[set3,set5],set7)
  ]

getAllEventsData :: [(String,EventSeq Text Rational,[Text])]
getAllEventsData =
  [ ("getAllEvents: empty",[],[])
  , ("getAllEvents: single",[(set3,["1"])],["1"])
  , ("getAllEvents: single1",[(set3,[])],["1","2"])
  , ("getAllEvents: all",[(set3,[]),(set4,[])],
     ["1;1","1;2","1;3","1;4","2;1","2;2","2;3","2;4"])
  , ("getAllEvents: norm",[(set3,["1"]),(set4,[])],
     ["1;1","1;2","1;3","1;4"])
  , ("getAllEvents: norm 1",[(set3,["1"]),(set4,["2","3"])],
     ["1;2","1;3"])
  , ("getAllEvents: norm 2",[(set3,["1","3"]),(set4,["2","3"])],
     ["1;2","1;3","3;2","3;3"])
  ]

conditionalProbData :: [(String,ProbSet Int Rational,[Int],[Int],Rational)]
conditionalProbData =
  [ ("conditionalProb: empty",M.empty,[],[],0)
  , ("conditionalProb: empty 1",set1,[],[1],0)
  , ("conditionalProb: empty 2",set2,[1],[],0)
  , ("conditionalProb: empty 3",M.empty,[],[],0)
  , ("conditionalProb: zero",set1,[1],[2],0)
  , ("conditionalProb: zero 1",set2,[2],[1,3],0)
  , ("conditionalProb: norm",set1,[1],[1,2],2%3)
  , ("conditionalProb: norm 1",set2,[2],[1..3],1%6)
  , ("conditionalProb: norm 2",set2,[1,2],[1,2,4],3%5)
  , ("conditionalProb: norm 3",set2,[3,4],[2..4],5%6)
  ]

bayesData :: [(String,ProbSet Int Rational,[Int],[Int],Rational)]
bayesData =
  [ ("bayes: empty",M.empty,[],[],0)
  , ("bayes: empty 1",set1,[],[1],0)
  , ("bayes: empty 2",set2,[1],[],0)
  , ("bayes: empty 3",M.empty,[],[],0)
  , ("bayes: zero",set1,[1],[2],0)
  , ("bayes: zero 1",set2,[2],[1,3],0)
  , ("bayes: norm",set1,[1],[1,2],2%3)
  , ("bayes: norm 1",set2,[2],[1..3],1%6)
  , ("bayes: norm 2",set2,[1,2],[1,2,4],3%5)
  , ("bayes: norm 3",set2,[3,4],[2..4],5%6)
  ]

densityFuncDataEmpty :: [(String,Int,Rational)]
densityFuncDataEmpty =
  [ ("densityFunc: empty",3,0)
  , ("densityFunc: empty 1",-4,0)
  ]

densityFuncDataSet1 :: [(String,Int,Rational)]
densityFuncDataSet1 =
  [ ("densityFunc: zero",0,0)
  , ("densityFunc: norm",1,2%3)
  , ("densityFunc: norm 1",2,1)
  , ("densityFunc: norm 2",7,1)
  ]

densityFuncDataSet2 :: [(String,Int,Rational)]
densityFuncDataSet2 =
  [ ("densityFunc: zero 1",-3,0)
  , ("densityFunc: norm 3",1,1%4)
  , ("densityFunc: norm 4",2,3%8)
  , ("densityFunc: norm 5",3,3%4)
  , ("densityFunc: norm 6",4,1)
  , ("densityFunc: norm 7",10,1)
  ]
