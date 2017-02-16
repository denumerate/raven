module Raven.Data.Stat
  ( mean
  , intMean
  , ratioMean
  , median
  , intMedian
  , ratioMedian
  , countInstances
  , mode
  , range
  )where

import Data.Ratio
import Data.List
import Data.Map (Map)
import qualified Data.Map as M

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

-- |Median for fractional values
median :: (Fractional a,Ord a) => [a] -> a
median [] = 0
median ls = let len = length ls
                ls' = sort ls
                pnt = div len 2
                in if odd len
                   then last $ take (pnt + 1) ls'
                   else mean $ snd $ splitAt (pnt-1) $ take (pnt+1) ls'

-- |Median for integral values
intMedian :: (Integral a) => [a] -> Ratio a
intMedian [] = 0
intMedian ls = let len = length ls
                   ls' = sort ls
                   pnt = div len 2
               in if odd len
                   then (last (take (pnt + 1) ls')) % 1
                   else intMean $ snd $ splitAt (pnt-1) $ take (pnt+1) ls'

-- |Median for ratios
ratioMedian :: (Integral a) => [Ratio a] -> Ratio a
ratioMedian [] = 0
ratioMedian ls = let len = length ls
                     ls' = sort ls
                     pnt = div len 2
                 in if odd len
                    then last (take (pnt + 1) ls')
                    else ratioMean $ snd $ splitAt (pnt-1) $ take (pnt+1) ls'

-- |Counts the number of times each item in a list occurs
countInstances :: (Ord a) => [a] -> Map a Int
countInstances = foldl' (\acc val -> case M.lookup val acc of
                            Just count -> M.insert val (succ count) acc
                            _ -> M.insert val 1 acc) M.empty

-- |Mode average, returns all highest values
mode :: (Ord a) => [a] -> [(a,Int)]
mode = foldl' findMaxs [] . M.toList . countInstances
  where
    findMaxs [] val = [val]
    findMaxs acc@((_,aVal):_) val@(_,vVal) = case compare aVal vVal of
                                               GT -> acc
                                               EQ -> val:acc
                                               _ -> [val]

-- |calculates the range of a list
range :: (Ord a) => [a] -> Maybe (a,a)
range [] = Nothing
range ls = Just (minimum ls, maximum ls)
