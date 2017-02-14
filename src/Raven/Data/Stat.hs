module Raven.Data.Stat
  ( mean
  , intMean
  , ratioMean
  , median
  )where

import Data.Ratio
import Data.List

-- |Mean for fractional values
mean :: (Fractional a) => [a] -> a
mean [] = 0
mean ls = sum ls / (fromIntegral . length) ls

-- |Mean for integral values
intMean :: (Integral a) => [a] -> Ratio a
intMean [] = 0
intMean ls = (sum) ls % (fromIntegral . length) ls

-- |Mean for ratios
ratioMean :: (Integral a) => [Ratio a] -> Ratio a
ratioMean [] = 0
ratioMean ls = let s = sum ls in
  numerator s % ((fromIntegral (length ls)) * (denominator s))

-- |median for fractional values
median :: (Fractional a,Ord a) => [a] -> a
median [] = 0
median ls = let len = length ls
                ls' = sort ls
                pnt = div len 2
                in if odd len
                   then last $ take (pnt + 1) ls'
                   else mean $ snd $ splitAt (pnt-1) $ take (pnt+1) ls'
