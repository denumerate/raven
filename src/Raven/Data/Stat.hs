module Raven.Data.Stat
  (
  )where

import Data.Vector(Vector)
import qualified Data.Vector as V
import Data.Typeable

import Raven.Data.Entry

entrySum :: (Entry a,Integral b,Typeable b) => TypeRep -> Vector a -> b
entrySum eType = V.foldl' (\acc val -> case (dumpEntry val :: Maybe eType) of
                          Just val' -> val' + acc
                          Nothing -> acc) 0
