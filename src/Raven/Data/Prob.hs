{-# LANGUAGE OverloadedStrings #-}
module Raven.Data.Prob
  ( ProbSet
  , EventSeq
  , isValidSet
  , subSetProb
  , eventSeqProb
  , combineProbSets
  , getAllEvents
  , conditionalProb
  )where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.List
import Data.Text (Text)
import qualified Data.Text as Text

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

-- |Calculates the probability of an EventSeq.
-- Returns 0 if any empty sets or a missing events.
-- Assumes valid ProbSets
-- (to avoid the possibility of rounding errors in the predicate).
eventSeqProb :: (Ord a,Num b) => EventSeq a b -> b
eventSeqProb = foldl' (\acc (pSet,subSet) -> acc * subSetProb pSet subSet) 1

-- |Combine a list of ProbSets into a single set.
-- Assumes valid ProbSets
-- (to avoid the possibility of rounding errors in the predicate).
combineProbSets :: (Num b) => [ProbSet Text b] -> ProbSet Text b
combineProbSets =
  Map.fromList . map (\(eName,eProb) -> (Text.concat (reverse eName),eProb)) .
  foldl' (\acc val ->
            concatMap (\(eName,eProb) ->
                          if null acc
                          then [([eName],eProb)]
                          else map (\(eName1,eProb1) ->
                                      (eName:";":eName1,eProb * eProb1)
                                      ) acc
                      ) (Map.toList val)
            ) []

-- |Gets all possible names of distinct events given an event sequence.
-- Treats empty lists as meaning all events are possible.
-- Includes missing events (be careful).
getAllEvents :: EventSeq Text b -> [Text]
getAllEvents = map (Text.concat . reverse) . foldl' combine []
  where
    combine :: [[Text]] -> (ProbSet Text b,[Text]) ->[[Text]]
    combine [] (pSet,[]) = map (\val -> [val]) $ Map.keys pSet
    combine [] (_,subSet) = map (\val -> [val]) subSet
    combine acc (pSet,[]) = let ks = Map.keys pSet in
      concatMap (\val -> map (\k -> k:";":val) ks) acc
    combine acc (_,subSet) =
      concatMap (\val -> map (\k -> k:";":val) subSet) acc

-- |Calculates the probability that the first event will happen if the second
-- event has happened, both events being subsets of the ProbSet.
-- Assumes a valid ProbSet
-- (to avoid the possibility of rounding errors in the predicate).
conditionalProb :: (Ord a,Fractional b) => ProbSet a b -> [a] -> [a] -> b
conditionalProb _ _ [] = 0
conditionalProb pSet e1 e2 = subSetProb pSet (intersect e1 e2) /
  subSetProb pSet e2
