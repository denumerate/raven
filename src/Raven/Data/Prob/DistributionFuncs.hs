module Raven.Data.Prob.DistributionFuncs
  ( uniformDistribution
  , intUniformDistribution
  , uniformDensity
  , intUniformDensity
  )where

import qualified Data.Map as Map
import Data.Ratio

import Raven.Data.Prob

-- |Creates a Uniform distribution function from a ProbSet.
-- Assumes a valid ProbSet
-- (to avoid the possibility of rounding errors in the predicate).
uniformDistribution :: (Fractional a,Ord a) => ProbSet a b -> (a -> a)
uniformDistribution pSet = let ks = Map.keys pSet
  in if null ks then (\_ -> 0)
     else let min' = minimum ks
              max' = maximum ks
          in (\val -> if val < min'  then 0
                      else if val >= max' then 1
                           else (val - min') / (max' - min'))

-- |Creates a Uniform distribution function from a ProbSet that uses Integral Values.
-- Assumes a valid ProbSet
-- (to avoid the possibility of rounding errors in the predicate).
intUniformDistribution :: (Integral a) => ProbSet a b -> (a -> Ratio a)
intUniformDistribution pSet = let ks = Map.keys pSet
  in if null ks then (\_ -> 0)
     else let min' = minimum ks
              max' = maximum ks
          in (\val -> if val < min' then 0
                      else if val >= max' then 1
                           else (val - min') % (max' - min'))

-- |Creates a Uniform density function from a ProbSet.
-- Assumes a valid ProbSet
-- (to avoid the possibility of rounding errors in the predicate).
uniformDensity :: (Fractional a,Ord a) => ProbSet a b -> (a -> a)
uniformDensity pSet = let ks = Map.keys pSet
  in if null ks then (\_ -> 0)
     else let min' = minimum ks
              max' = maximum ks
          in (\val -> if val < min' || val > max' then 0
                      else 1 / (max' - min'))

-- |Creates a Uniform density function from a ProbSet that uses Integral Values.
-- Assumes a valid ProbSet
-- (to avoid the possibility of rounding errors in the predicate).
intUniformDensity :: (Integral a) => ProbSet a b -> (a -> Ratio a)
intUniformDensity pSet = let ks = Map.keys pSet
  in if null ks then (\_ -> 0)
     else let min' = minimum ks
              max' = maximum ks
          in (\val -> if val < min' || val > max' then 0
                      else 1 % (max' - min'))
