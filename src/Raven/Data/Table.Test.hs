{-# LANGUAGE OverloadedStrings #-}
module Main(main) where

import Test.HUnit
import System.Exit
import Data.Vector (Vector)
import Data.Text (Text)
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
  ++ runtests2 getColByIndex getColByIndexData
  ++ runtests2 getColByTitle getColByTitleData
  ++ runtests3 addColumn addColumnData

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

--run tests with 3 input
runtests3 :: (Eq d,Show d) => (a -> b -> c -> d) -> [(String,a,b,c,d)] -> [Test]
runtests3 f ls = map (uncurry (~:)) (createtest3 f ls)

--create tests with 3 input
createtest3 :: (Eq d,Show d) => (a -> b -> c -> d) -> [(String,a,b,c,d)] -> [(String,Test)]
createtest3 f = map (\(s,w,x,y,z) -> (s,TestCase $ z @=? f w x y))


--Test values:
titleError = Right "Number of titles does not match number of columns"
colLengthError = Right "Not all columns the same length"

single = V.fromList [V.fromList [1]]

test1 = V.fromList
  [ V.fromList [1,2]
  , V.fromList [1]
  ]

test2 = V.fromList
  [ V.fromList [2,5]
  , V.empty
  ]

test3 = V.fromList
  [ V.fromList [1,2,3]
  , V.fromList [4,5,6]
  , V.fromList [7]
  ]

buildTableData :: [(String, Titles, Vector (Vector Int), Either (Table a) Error)]
buildTableData =
  [ ("buildTable: empty",V.empty,V.empty,Left empty)
  , ("buildTable: title error",V.empty,single,titleError)
  , ("buildTable: title error 1",V.fromList ["blah"],V.empty,titleError)
  , ("buildTable: title error 2",V.fromList ["blah","bleh"],single
    ,titleError)
  , ("buildTable: length error",V.fromList ["1","2"],test1,colLengthError)
  , ("buildTable: length error 1",V.fromList ["1","2"],test2,colLengthError)
  , ("buildTable: length error 2",V.fromList ["1","2","3"],test3,colLengthError)
  ]

getColByIndexData :: [(String, Table Int, Int, Either (Vector Int) Error)]
getColByIndexData =
  []

getColByTitleData :: [(String, Table Int, Text, Either (Vector Int) Error)]
getColByTitleData =
  []

addColumnData :: [(String, Table Int, Text, Vector Int, Either (Table Int) Error)]
addColumnData =
  []
