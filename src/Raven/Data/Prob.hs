module Raven.Data.Prob
  ( ProbSet
  , EventSeq
  , isValidSet
  , subSetProb
  )where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.List

-- |A set of events mapped to a probability value
type ProbSet a b = Map a b

-- |Represents a possible sequence of events.
-- Pairs a ProbSet with a subset of events representing the possible events from that set.
-- Goes in order.
type EventSeq a b = [(ProbSet a b,[a])]

-- |Checks if the set is valid
-- (all probabilities add to one and all [0,1]).
isValidSet :: (Num b,Ord b) => ProbSet a b -> Bool
isValidSet pSet = (1 == Map.foldl' (+) 0 pSet) &&
  (all (\n -> n >= 0 && n <= 1) (Map.elems pSet))

-- |Calculates the probability of a subset of events happening.
-- Returns 0 if empty set or a missing event.
-- Assumes a valid ProbSet
-- (to avoid the possibility of rounding errors in the predicate).
subSetProb :: (Ord a,Num b) => ProbSet a b -> [a] -> b
subSetProb pSet subSet =
  case foldl' (\acc val -> acc >>=
              (\acc' -> Map.lookup val pSet >>=
              return . (+acc'))) (Just 0) subSet of
    Just p -> p
    _ -> 0
