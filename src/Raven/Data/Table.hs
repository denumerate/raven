{-# LANGUAGE OverloadedStrings #-}
module Raven.Data.Table
  ( Titles
  , Table
  , Error
  , Raven.Data.Table.null
  , empty
  , buildTable
  , getColByIndex
  , getColByTitle
  , addColumn
  ) where

import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Text (Text)

-- |Table is the simplest possible implementation
-- Typeclass requirements w/in functions
-- Insures that all vectors are the same length
-- Titles are column titles
type Titles = Vector Text
data Table a = Table Titles (Vector (Vector a))
  deriving(Eq, Show)

type Error = Text

-- |Create an empty table
empty :: Table a
empty = Table V.empty V.empty

-- |Build a table from Vectors
-- Fails if all the vectors are not the same length
-- or there are not the same number of titles as
buildTable :: Titles -> Vector (Vector a) -> Either (Table a) Error
buildTable titles vectors
  |V.null titles && V.null vectors = Left empty
  |V.null titles || V.null vectors = Right "Number of titles does not match number of columns"
  |V.length titles == V.length vectors =
     if all (\vec -> V.length vec == (V.length . V.head) vectors) $ V.tail vectors
        then Left $ Table titles vectors
        else Right "Not all columns the same length"
  |otherwise = Right "Number of titles does not match number of columns"

-- |Pulls a column from the table by column index
-- O(1)
getColByIndex :: Table a -> Int -> Either (Vector a) Error
getColByIndex (Table _ tab) ind = case tab V.!? ind of
  Just vec -> Left vec
  _ -> Right "Index out of bounds"

-- |Pulls a column from the table by title
-- O(n) where n is the number of columns
getColByTitle :: Table a -> Text -> Either (Vector a) Error
getColByTitle tab@(Table ttls _) ttl = case V.findIndex (\val -> val == ttl) ttls of
  Just ind -> getColByIndex tab ind
  _ -> Right "Column title not found"

-- |Adds a column to the end
-- O(n) where n is the number of columns
addColumn :: Table a -> Text -> Vector a -> Either (Table a) Error
addColumn t@(Table ttls tab) ttl vec
  |Raven.Data.Table.null t = buildTable (V.fromList [ttl]) (V.fromList [vec])
  |(V.length . V.head) tab == V.length vec =
     Left $ Table (V.snoc ttls ttl) (V.snoc tab vec)
  |otherwise = Right "Vector wrong length"

-- |Test if a table is empty O(1)
null :: Table a -> Bool
null (Table ttls _) = V.null ttls
