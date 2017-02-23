module Main(main) where

import Test.HUnit
import System.Exit
import qualified Data.Map as M

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

subSetProbData :: [(String,ProbSet Int Rational,[Int],Rational)]
subSetProbData =
  []
