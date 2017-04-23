{-# LANGUAGE OverloadedStrings #-}
module Raven.Data.BasicEntry
  ( BasicEntry (..)
  )where

import Data.Ratio
import qualified Data.Text as Text
import Data.Text (Text)
import Data.Typeable
import qualified Data.Vector as V
import Text.Read

import Raven.Data.Entry
import Raven.Data.Stat

-- |BasicEntry creates a simple entry with bounded values
data BasicEntry = BasicInt Int
                | BasicDouble Double
                | BasicRatio (Ratio Int)
                | BasicString Text
                | BasicBool Bool
                | BasicNA

instance Entry BasicEntry where
  readEntry val
    |(readMaybe val :: Maybe Int) /= Nothing = BasicInt $ read val
    |(readMaybe val :: Maybe Double) /= Nothing = BasicDouble $ read val
    |(readMaybe val :: Maybe (Ratio Int)) /= Nothing = BasicRatio $ read val
    |(readMaybe val :: Maybe Bool) /= Nothing = BasicBool $ read val
    |val == "NA" = BasicNA
    |otherwise = BasicString $ Text.pack val

  getEntry (BasicInt val) = cast val
  getEntry (BasicDouble val) = cast val
  getEntry (BasicRatio val) = cast val
  getEntry (BasicString val) = cast val
  getEntry (BasicBool val) = cast val
  getEntry _ = Nothing

  isEntryNA BasicNA = True
  isEntryNA _ = False

  na = BasicNA

  summary vs
    |V.null vs = "Empty vector"
    |not (null (getEntries vs :: [Int])) = let vs' = (getEntries vs :: [Int]) in
       Text.concat [ "Mean: ", (Text.pack . show . intMean) vs'
                   , "\nMedian: ", (Text.pack . show . intMedian) vs'
                   , "\nMode: ", (Text.pack . show . mode) vs'
                   , "\nMinimum: ", (Text.pack . show . minimum) vs'
                   , "\nMaximum: ", (Text.pack . show . maximum) vs'
                   , "\nStandard Deviation: "
                   , (Text.pack . show . intStdDeviation) vs'
                   , "\nNumber of NA's: ", (Text.pack . show . countNAs) vs
                   , "\nNumber of Entries: ", (Text.pack . show . V.length) vs
                   ]
    |not (null (getEntries vs :: [Double])) =
       let vs' = (getEntries vs :: [Double]) in
         Text.concat [ "Mean: ", (Text.pack . show . mean) vs'
                     , "\nMedian: ", (Text.pack . show . median) vs'
                     , "\nMode: ", (Text.pack . show . mode) vs'
                     , "\nMinimum: ", (Text.pack . show . minimum) vs'
                     , "\nMaximum: ", (Text.pack . show . maximum) vs'
                     , "\nStandard Deviation: "
                     , (Text.pack . show . stdDeviation) vs'
                     , "\nNumber of NA's: ", (Text.pack . show . countNAs) vs
                     , "\nNumber of Entries: ", (Text.pack . show . V.length) vs
                     ]
    |not (null (getEntries vs :: [Ratio Int])) =
       let vs' = (getEntries vs :: [Ratio Int]) in
         Text.concat [ "Mean: ", (Text.pack . show . mean) vs'
                     , "\nMedian: ", (Text.pack . show . median) vs'
                     , "\nMode: ", (Text.pack . show . mode) vs'
                     , "\nMinimum: ", (Text.pack . show . minimum) vs'
                     , "\nMaximum: ", (Text.pack . show . maximum) vs'
                     , "\nStandard Deviation: "
                     , (Text.pack . show . ratioStdDeviation) vs'
                     , "\nNumber of NA's: ", (Text.pack . show . countNAs) vs
                     , "\nNumber of Entries: ", (Text.pack . show . V.length) vs
                     ]
    |not (null (getEntries vs :: [Bool])) =
       let vs' = (getEntries vs :: [Bool]) in
         Text.concat [ "Breakdown: ", (Text.pack . show . countInstances) vs'
                     , "\nNumber of NA's: ", (Text.pack . show . countNAs) vs
                     , "\nNumber of Entries: ", (Text.pack . show . V.length) vs
                     ]
    |otherwise =
       let vs' = (getEntries vs :: [Text]) in
         Text.concat [ "Top Entry: ", (Text.pack . show . mode) vs'
                     , "\nNumber of NA's: ", (Text.pack . show . countNAs) vs
                     , "\nNumber of Entries: ", (Text.pack . show . V.length) vs
                     ]
