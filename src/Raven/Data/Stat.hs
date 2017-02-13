module Raven.Data.Stat
  ( entrySum
  )where

import Data.Vector(Vector)
import qualified Data.Vector as V

import Raven.Data.Entry

-- |Pulls the entry using the provided function and sums the result
entrySum :: (Entry a,Num b) =>  Vector a -> b
entrySum = V.foldl' (\acc val -> case getEntryNum val of
                          Just val' -> val' + acc
                          Nothing -> acc) 0
