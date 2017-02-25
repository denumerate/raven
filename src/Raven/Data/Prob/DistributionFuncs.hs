module Raven.Data.Prob.DistributionFuncs
  ( uniform
  , intUniform
  )where

import qualified Data.Map as Map

import Raven.Data.Prob

-- |Creates a uniform distribution function from a ProbSet.
-- Assumes a valid ProbSet
-- (to avoid the possibility of rounding errors in the predicate).
uniform :: (Fractional a,Ord a) => ProbSet a a -> (a -> a)
uniform pSet = let ks = Map.keys pSet
                   min' = minimum ks
                   max' = maximum ks
  in (\val -> if val < min' || val > max'
              then 0
              else 1 / (max' - min'))

-- |Creates a uniform distribution function from a ProbSet (for integral values).
-- Assumes a valid ProbSet
-- (to avoid the possibility of rounding errors in the predicate).
intUniform :: (Integral a,Fractional b) => ProbSet a b -> (a -> b)
intUniform pSet = let ks = Map.keys pSet
                      min' = minimum ks
                      max' = maximum ks
  in (\val -> if val < min' || val > max'
              then 0
              else 1 / fromIntegral (max' - min'))
