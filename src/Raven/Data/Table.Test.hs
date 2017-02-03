{-# LANGUAGE OverloadedStrings #-}
module Main(main) where

import Test.HUnit
import System.Exit
import Data.Vector (Vector)
import qualified Data.Vector as V

import Raven.Data.Table

main :: IO ()
main = do
  cs@(Counts _ _ errs fails) <- runTestTT allTests
  putStrLn (showCounts cs)
  if errs > 0 || fails > 0
    then exitFailure
    else exitSuccess

allTests :: Test
allTests = TestList $
  runtests2 buildTable buildTableData

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
titleError = Right "Number of titles does not match number of columns"

single = V.fromList [V.fromList [1]]

buildTableData :: [(String,Titles, Vector (Vector Int), Either (Table a) Error)]
buildTableData =
  [ ("buildTable: empty",V.empty,V.empty,Left empty)
  , ("buildTable: title error",V.empty,single,titleError)
  , ("buildTable: title error 1",V.fromList ["blah"],V.empty,titleError)
  , ("buildTable: title error 2",V.fromList ["blah","bleh"],single
    ,titleError)
  , ("buildTable: length error",)
  ]
