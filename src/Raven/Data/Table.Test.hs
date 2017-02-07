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
  ++ runtests2 dropColsByIndex dropColsByIndexData
  ++ runtests2 dropColsByIndex' dropColsByIndex'Data
  ++ runtests2 dropColsByTitle dropColsByTitleData
  ++ runtests2 dropColsByTitle' dropColsByTitle'Data
  ++ runtests2 combineTableByCols combineTableByColsData
  ++ runtests2 takeRows takeRowsData
  ++ runtests2 dropRows dropRowsData

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
nRowsError = Right "Tables must have equal number of rows"

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

table6 = buildTable (V.fromList ["2","3"])
  (V.fromList
    [ V.fromList [4,5,6]
    , V.fromList [7,8,9]
    ])

table7 = buildTable (V.fromList ["1","2"])
  (V.fromList
    [ V.fromList [1,2,3]
    , V.fromList [4,5,6]
    ])

table8 = buildTable (V.fromList ["2"])
  (V.fromList
    [ V.fromList [4,5,6]
    ])

table9 = buildTable (V.fromList ["1","2","3","1","2"])
  (V.fromList
    [ V.fromList [1,2,3]
    , V.fromList [4,5,6]
    , V.fromList [7,8,9]
    , V.fromList [1,2,3]
    , V.fromList [4,5,6]
    ])

table10 = buildTable (V.fromList ["2","3","1","2","3"])
  (V.fromList
    [ V.fromList [4,5,6]
    , V.fromList [7,8,9]
    , V.fromList [1,2,3]
    , V.fromList [4,5,6]
    , V.fromList [7,8,9]
    ])

table11 = buildTable (V.fromList ["1","2","3"])
  (V.fromList [V.empty,V.empty,V.empty])

table12 = buildTable (V.fromList ["1","2","3"])
  (V.fromList
    [ V.fromList [1,2]
    , V.fromList [4,5]
    , V.fromList [7,8]
    ])

table13 = buildTable (V.fromList ["1","2","3"])
  (V.fromList
    [ V.fromList [3]
    , V.fromList [6]
    , V.fromList [9]
    ])


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
  [ ("dropColByIndex: empty",empty,0,indError)
  , ("dropColByIndex: index error",unpackTable table1,-1,indError)
  , ("dropColByIndex: index error 1",unpackTable table1,3,indError)
  , ("dropColByIndex: single",unpackTable single',0,Left empty)
  , ("dropColByIndex: norm",unpackTable table1,0,table6)
  , ("dropColByIndex: norm 1",unpackTable table1,2,table7)
  ]

dropColByTitleData :: [(String, Table Int, Text, Either (Table Int) Error)]
dropColByTitleData =
  [ ("dropColByTitle: empty",empty,"sldkfj",ttlError)
  , ("dropColByTitle: title error",unpackTable table1,"sldkfj",ttlError)
  , ("dropColByTitle: single",unpackTable single',"1",Left empty)
  , ("dropColByTitle: norm",unpackTable table1,"1",table6)
  , ("dropColByTitle: norm 1",unpackTable table1,"3",table7)
  ]

dropColsByIndexData :: [(String, Table Int, [Int], Table Int)]
dropColsByIndexData =
  [ ("dropColsByIndex: empty", empty, [], empty)
  , ("dropColsByIndex: empty 1", empty, [-1,0,2], empty)
  , ("dropColsByIndex: single",unpackTable single', [0], empty)
  , ("dropColsByIndex: single 1",unpackTable single', [-1,0,2], empty)
  , ("dropColsByIndex: single 2",unpackTable single',[-1,1,2],unpackTable single')
  , ("dropColsByIndex: single 3",unpackTable single',[],unpackTable single')
  , ("dropColsByIndex: norm",unpackTable table1,[-1,3,4],unpackTable table1)
  , ("dropColsByIndex: norm 1",unpackTable table1,[0..2],empty)
  , ("dropColsByIndex: norm 2",unpackTable table1,[0..4],empty)
  , ("dropColsByIndex: norm 3",unpackTable table1, [0], unpackTable table6)
  , ("dropColsByIndex: norm 4",unpackTable table1, [0,4], unpackTable table6)
  , ("dropColsByIndex: norm 5",unpackTable table1, [0,2],unpackTable table8)
  , ("dropColsByIndex: norm 6",unpackTable table1, [0,2,3],unpackTable table8)
  , ("dropColsByIndex: norm 7",unpackTable table1, [],unpackTable table1)
  ]

dropColsByIndex'Data :: [(String, Table Int, [Int], Either (Table Int) Error)]
dropColsByIndex'Data =
  [ ("dropColsByIndex': empty", empty, [], Left empty)
  , ("dropColsByIndex': empty 1", empty, [-1,0,2], indError)
  , ("dropColsByIndex': single",unpackTable single', [0],Left empty)
  , ("dropColsByIndex': single 1",unpackTable single', [-1,0,2], indError)
  , ("dropColsByIndex': single 2",unpackTable single',[-1,1,2],indError)
  , ("dropColsByIndex': single 3",unpackTable single',[],single')
  , ("dropColsByIndex': norm",unpackTable table1,[-1,3,4],indError)
  , ("dropColsByIndex': norm 1",unpackTable table1,[0..2],Left empty)
  , ("dropColsByIndex': norm 2",unpackTable table1,[0..4],indError)
  , ("dropColsByIndex': norm 3",unpackTable table1, [0], table6)
  , ("dropColsByIndex': norm 4",unpackTable table1, [0,4], indError)
  , ("dropColsByIndex': norm 5",unpackTable table1, [2,0], table8)
  , ("dropColsByIndex': norm 6",unpackTable table1, [0,2,3],indError)
  , ("dropColsByIndex': norm 7",unpackTable table1, [], table1)
  ]

dropColsByTitleData :: [(String, Table Int, [Text], Table Int)]
dropColsByTitleData =
  [ ("dropColsByTitle: empty", empty, [], empty)
  , ("dropColsByTitle: empty 1", empty, ["1","3","dk"], empty)
  , ("dropColsByTitle: single",unpackTable single', ["1"], empty)
  , ("dropColsByTitle: single 1",unpackTable single', ["1","3","dk"], empty)
  , ("dropColsByTitle: single 2",unpackTable single', ["-1","3","dk"],
     unpackTable single')
  , ("dropColsByTitle: single 3",unpackTable single',[],unpackTable single')
  , ("dropColsByTitle: norm",unpackTable table1,["0","4","sdk"],
     unpackTable table1)
  , ("dropColsByTitle: norm 1",unpackTable table1,["1","2","3"],empty)
  , ("dropColsByTitle: norm 2",unpackTable table1,["1","6","2","3"],empty)
  , ("dropColsByTitle: norm 3",unpackTable table1, ["1"], unpackTable table6)
  , ("dropColsByTitle: norm 4",unpackTable table1, ["1","t"], unpackTable table6)
  , ("dropColsByTitle: norm 5",unpackTable table1, ["1","3"],unpackTable table8)
  , ("dropColsByTitle: norm 6",unpackTable table1, ["3","1","g"],
     unpackTable table8)
  , ("dropColsByTitle: norm 7",unpackTable table1, [],unpackTable table1)
  ]


dropColsByTitle'Data :: [(String, Table Int, [Text], Either (Table Int) Error)]
dropColsByTitle'Data =
  [ ("dropColsByTitle': empty", empty, [],Left empty)
  , ("dropColsByTitle': empty 1", empty, ["1","3","dk"], ttlError)
  , ("dropColsByTitle': single",unpackTable single', ["1"],Left empty)
  , ("dropColsByTitle': single 1",unpackTable single', ["1","3","dk"], ttlError)
  , ("dropColsByTitle': single 2",unpackTable single', ["-1","3","dk"],
     ttlError)
  , ("dropColsByTitle': single 3",unpackTable single',[], single')
  , ("dropColsByTitle': norm",unpackTable table1,["0","4","sdk"],
     ttlError)
  , ("dropColsByTitle': norm 1",unpackTable table1,["1","2","3"],Left empty)
  , ("dropColsByTitle': norm 2",unpackTable table1,["1","6","2","3"],ttlError)
  , ("dropColsByTitle': norm 3",unpackTable table1, ["1"], table6)
  , ("dropColsByTitle': norm 4",unpackTable table1, ["1","t"], ttlError)
  , ("dropColsByTitle': norm 5",unpackTable table1, ["1","3"], table8)
  , ("dropColsByTitle': norm 6",unpackTable table1, ["3","1","g"],
     ttlError)
  , ("dropColsByTitle': norm 7",unpackTable table1, [], table1)
  ]

combineTableByColsData :: [(String, Table Int, Table Int, Either (Table Int) Error)]
combineTableByColsData =
  [ ("combineTableByCols: empty",empty,empty,Left empty)
  , ("combineTableByCols: empty 1",empty,unpackTable table1,nRowsError)
  , ("combineTableByCols: empty 2",unpackTable table3,empty,nRowsError)
  , ("combineTableByCols: error",unpackTable table1,unpackTable single',
    nRowsError)
  , ("combineTableByCols: norm",unpackTable table1,unpackTable table7,
    table9)
  , ("combineTableByCols: norm 1",unpackTable table6, unpackTable table1,
    table10)
  ]

takeRowsData :: [(String,Table Int,Int,Table Int)]
takeRowsData =
  [ ("takeRows: empty", empty, 0, empty)
  , ("takeRows: empty 1", empty, 10, empty)
  , ("takeRows: empty 2", empty, -2, empty)
  , ("takeRows: 0",unpackTable table1,0,unpackTable table11)
  , ("takeRows: all",unpackTable table1,3,unpackTable table1)
  , ("takeRows: all+",unpackTable table1,4,unpackTable table1)
  , ("takeRows: norm",unpackTable table1,2,unpackTable table12)
  ]

dropRowsData :: [(String,Table Int,Int,Table Int)]
dropRowsData =
  [ ("dropRows: empty", empty, 0, empty)
  , ("dropRows: empty 1", empty, 10, empty)
  , ("dropRows: empty 2", empty, -2, empty)
  , ("dropRows: 0",unpackTable table1,0,unpackTable table1)
  , ("dropRows: all",unpackTable table1,3,unpackTable table11)
  , ("dropRows: all+",unpackTable table1,4,unpackTable table11)
  , ("dropRows: norm",unpackTable table1,2,unpackTable table13)
  ]
