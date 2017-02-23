module Main(main) where

import Test.HUnit
import System.Exit
import qualified Data.Map as M
import Data.Ratio

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

--run tests with 1 input
runtests :: (Eq b,Show b) => (a -> b) -> [(String,a,b)] -> [Test]
runtests f ls = map (uncurry (~:)) (createtest f ls)

--create tests with 1 input
createtest :: (Eq b,Show b) => (a -> b) -> [(String,a,b)] -> [(String,Test)]
createtest f = map (\(s,x,y) -> (s,TestCase $ y @=? f x))

--run tests with 2 input
runtests2 :: (Eq c,Show c) => (a -> b -> c) -> [(String,a,b,c)] -> [Test]
runtests2 f ls = map (uncurry (~:)) (createtest2 f ls)

--create tests with 2 input
createtest2 :: (Eq c,Show c) => (a -> b -> c) -> [(String,a,b,c)] -> [(String,Test)]
createtest2 f = map (\(s,x,y,z) -> (s,TestCase $ z @=? f x y))

--Test values:
set1 = M.fromList [(1,2%3),(2,1%3)]

set2 = M.fromList [(1,1%4),(2,1%8),(3,3%8),(4,1%4)]

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
