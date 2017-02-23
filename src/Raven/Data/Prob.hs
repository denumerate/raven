module Raven.Data.Prob
  ( ProbSet
  , RatioProbSet
  , isValidSet
  )where

import Data.Map (Map)
import qualified Data.Map as Map

-- |A set of events mapped to a probability value represented as a double
type ProbSet a = Map a Double

-- |A set of events mapped to a probability value represented as a Rational
type RatioProbSet a = Map a Rational

-- |Checks if the set is valid (all probabilities add to one).
-- Beware of rounding errors.
isValidSet :: ProbSet a -> Bool
isValidSet = (1 ==) . Map.foldl' (+) 0
