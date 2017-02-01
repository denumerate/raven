module Raven.Data.Table
  ( Titles
  , Table
  , empty
  , buildEntry
  ) where

import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Text (Text)
import Raven.Data.Entry (Entry)

-- |Table is the simplest possible implementation
-- Typeclass requirements w/in functions
-- Insures that all vectors are the same length
-- Titles are column titles
type Titles = Vector Text
data Table a = Table Titles (Vector (Vector a))

type Error = String

-- |Create an empty table
empty :: (Entry a) => Table a
empty = Table V.empty V.empty

-- |Build a table from Vectors
-- Fails if all the vectors are not the same length
-- or there are not the same number of titles as
bulidTable :: (Entry a) => Titles -> Vector (Vector a) -> Either (Table a) Error
buildEntry titles vectors
  |V.length titles == V.length vectors &&
   all (\vec -> V.length vec == (V.length . V.head) vectors) $ V.tail vectors =
     Just $ Table titles vectors
  |otherwise = Nothing
