module Raven.Data.Table () where

import Data.Vector (Vector)
import qualified Data.Vector as V

type Table a = Vector (Vector a)
