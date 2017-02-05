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
  ++ runtests2 dropColByIndex dropColByIndexData
  ++ runtests2 dropColByTitle dropColByTitleData

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
indError = Right "Index out of bounds"
ttlError = Right "Column title not found"
addError = Right "Vector wrong length"

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

single' = buildTable (V.fromList ["1"]) single

table1 = buildTable (V.fromList ["1","2","3"])
  (V.fromList
    [ V.fromList [1,2,3]
    , V.fromList [4,5,6]
    , V.fromList [7,8,9]
    ])

table2 = buildTable (V.fromList ["1","s"])
  (V.fromList
    [ V.fromList [1]
    , V.fromList [0]
    ])

table3 = buildTable (V.fromList ["1","2","3","r"])
  (V.fromList
    [ V.fromList [1,2,3]
    , V.fromList [4,5,6]
    , V.fromList [7,8,9]
    , V.fromList [0,0,0]
    ])

table4 = buildTable (V.fromList ["4"])
  (V.fromList [V.fromList [6,7]])

table5 = buildTable (V.fromList ["kdj"])
  (V.fromList [V.empty])

unpackTable :: Either (Table a) Error -> Table a
unpackTable t = let Left t' = t
                in t'

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
  [ ("getColByIndex: empty",empty,0,indError)
  , ("getColByIndex: empty 1",empty,-4,indError)
  , ("getColByIndex: empty 2",empty,5,indError)
  , ("getColByIndex: index error",unpackTable single',-1,indError)
  , ("getColByIndex: index error 1",unpackTable single',1,indError)
  , ("getColByIndex: index error 2",unpackTable table1, -1,indError)
  , ("getColByIndex: index error 3",unpackTable table1,3,indError)
  , ("getColByIndex: single",unpackTable single',0,Left $ V.fromList [1])
  , ("getColByIndex: norm",unpackTable table1,0,Left $ V.fromList [1,2,3])
  , ("getColByIndex: norm 1",unpackTable table1,2,Left $ V.fromList [7,8,9])
  ]

getColByTitleData :: [(String, Table Int, Text, Either (Vector Int) Error)]
getColByTitleData =
  [ ("getColByTitle: empty",empty,"skjfs",ttlError)
  , ("getColByTitle: title error",unpackTable table1,"sldkfj",ttlError)
  , ("getColByTitle: single",unpackTable single',"1",Left $ V.fromList [1])
  , ("getColByTitle: norm",unpackTable table1,"2",Left $ V.fromList [4,5,6])
  , ("getColByTitle: norm 1",unpackTable table1,"3",Left $ V.fromList [7,8,9])
  ]

addColumnData :: [(String, Table Int, Text, Vector Int, Either (Table Int) Error)]
addColumnData =
  [ ("addColumn: empty",empty,"4",V.fromList [6,7],table4)
  , ("addColumn: empty 1",empty,"kdj",V.empty,table5)
  , ("addColumn: size error",unpackTable single',"k",V.empty,addError)
  , ("addColumn: size error 1",unpackTable table1,"d",V.fromList [2,4],
     addError)
  , ("addColumn: size error 2",unpackTable table1,"e",V.fromList [3,4,5,6],
     addError)
  , ("addColumn: single",unpackTable single',"s",V.fromList [0],
     Left $ unpackTable table2)
  , ("addColumn: norm",unpackTable table1,"r",V.fromList [0,0,0],
     Left $ unpackTable table3)
  ]

dropColByIndexData :: [(String, Table Int, Int, Either (Table Int) Error)]
dropColByIndexData =
  []

dropColByTitleData :: [(String, Table Int, Text, Either (Table Int) Error)]
dropColByTitleData =
  []
