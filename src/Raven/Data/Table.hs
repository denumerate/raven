{-# LANGUAGE OverloadedStrings #-}
module Raven.Data.Table
  ( Titles
  , Table
  , Error
  , Raven.Data.Table.null
  , nCols
  , empty
  , buildTable
  , getColByIndex
  , getColByTitle
  , addColumn
  , dropColByIndex
  , dropColByTitle
  , dropColsByIndex
  , dropColsByIndex'
  , dropColsByTitle
  , dropColsByTitle'
  ) where

import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Text (Text)

type Titles = Vector Text

-- |Table is the simplest possible implementation.
-- Insures that all vectors are the same length.
-- Titles are column titles
data Table a = Table Titles (Vector (Vector a))
  deriving(Eq, Show)

type Error = Text

-- Error Messages:
columnLengthError :: Text
columnLengthError = "Not all columns the same length"

titleNumberError :: Text
titleNumberError = "Number of titles does not match number of columns"

outOfBoundsError :: Text
outOfBoundsError = "Index out of bounds"

titleNotFoundError :: Text
titleNotFoundError = "Column title not found"

vectorLengthError :: Text
vectorLengthError = "Vector wrong length"

-- |Create an empty table
empty :: Table a
empty = Table V.empty V.empty

-- |Build a table from Vectors
-- Fails if all the vectors are not the same length.
-- or there are not the same number of titles as
buildTable :: Titles -> Vector (Vector a) -> Either (Table a) Error
buildTable titles vectors
  |V.null titles && V.null vectors = Left empty
  |V.null titles || V.null vectors = Right titleNumberError
  |V.length titles == V.length vectors =
     if all (\vec -> V.length vec == (V.length . V.head) vectors) $ V.tail vectors
        then Left $ Table titles vectors
        else Right columnLengthError
  |otherwise = Right titleNumberError

-- |Pulls a column from the table by column index
-- O(1)
getColByIndex :: Table a -> Int -> Either (Vector a) Error
getColByIndex (Table _ tab) ind = case tab V.!? ind of
  Just vec -> Left vec
  _ -> Right outOfBoundsError

-- |Pulls a column from the table by title.
-- O(n) where n is the number of columns
getColByTitle :: Table a -> Text -> Either (Vector a) Error
getColByTitle tab@(Table ttls _) ttl = case V.findIndex (\val -> val == ttl) ttls of
  Just ind -> getColByIndex tab ind
  _ -> Right titleNotFoundError

-- |Adds a column to the end.
-- O(n) where n is the number of columns
addColumn :: Table a -> Text -> Vector a -> Either (Table a) Error
addColumn t@(Table ttls tab) ttl vec
  |Raven.Data.Table.null t = buildTable (V.fromList [ttl]) (V.fromList [vec])
  |(V.length . V.head) tab == V.length vec =
     Left $ Table (V.snoc ttls ttl) (V.snoc tab vec)
  |otherwise = Right vectorLengthError

-- |Test if a table is empty O(1)
null :: Table a -> Bool
null (Table ttls _) = V.null ttls

-- |Returns the number of columns in a table O(1)
nCols :: Table a -> Int
nCols (Table ttls _) = V.length ttls

-- |Removes a single column from a table.
-- O(n) where n is the number of columns.
dropColByIndex :: Table a -> Int -> Either (Table a) Error
dropColByIndex t@(Table ttls cols) ind
  |ind < 0 ||
   ind >= nCols t ||
   Raven.Data.Table.null t = Right outOfBoundsError
  |otherwise = Left $ Table (V.ifilter ipred ttls) (V.ifilter ipred cols)
  where
    ipred i _ = i /= ind

-- |Removes a single column from a table.
-- O(n) where n is the number of columns.
-- approximately twice as slow as by index.
dropColByTitle :: Table a -> Text -> Either (Table a) Error
dropColByTitle t@(Table ttls _) ttl =
  case V.findIndex (\val -> val == ttl) ttls of
    Just ind -> dropColByIndex t ind
    _ -> Right titleNotFoundError

-- |takes a list of indices and filters out the associated columns.
-- Does not return an error if an index is missing.
-- O(mn) where n is the number of columns and m is the number of indices.
dropColsByIndex :: Table a -> [Int] -> Table a
dropColsByIndex (Table ttls cols) inds =
  Table (V.ifilter ipred ttls) (V.ifilter ipred cols)
  where
    ipred i _ = elem i inds

-- |takes a list of indices and filters out the associated columns.
-- Does return an error if an index is missing.
-- O(mn) where n is the number of columns and m is the number in indices
dropColsByIndex' :: Table a -> [Int] -> Either (Table a) Error
dropColsByIndex' t inds
  |all (\i -> i >= 0 && i < nCols t) inds = Left $ dropColsByIndex t inds
  |otherwise = Right outOfBoundsError

-- |takes a list of title and filters out the associated columns.
-- Does not return an error if a title is missing.
-- O(mn) where n is the number of columns and m is the number ttls looked forces.
-- Consistently slower than Indices version
dropColsByTitle :: Table a -> [Text] -> Table a
dropColsByTitle t@(Table ttls _) ttls' =
  dropColsByIndex t $ V.toList $
  V.findIndices  (`elem` ttls') ttls

-- |takes a list of title and filters out the associated columns.
-- Does return an error if a title is missing.
-- O(mn) where n is the number of columns and m is the number ttls looked forces.
-- Consistently slower than Indices version
dropColsByTitle' :: Table a -> [Text] -> Either (Table a) Error
dropColsByTitle' t@(Table ttls _) ttls' =
  let inds = V.toList $ V.findIndices  (`elem` ttls') ttls
  in if length ttls == length inds
     then Left $ dropColsByIndex t inds
     else Right $ titleNotFoundError
