{-# LANGUAGE OverloadedStrings #-}
module Raven.Data.BasicUnboundEntry
  ( BasicUnboundEntry(..)
  )where

import Data.Text (Text)
import qualified Data.Text as Text
import Text.Read
import Data.Typeable
import qualified Data.Vector as V
import Data.Ratio
import Data.Maybe

import Raven.Data.Entry
import Raven.Data.Stat

-- | BasicUnboundEntry creates a simple entry with unbounded values
data BasicUnboundEntry = BasicUnboundInt Integer
                       | BasicUnboundDouble Double
                       | BasicUnboundRatio Rational
                       | BasicUnboundString Text
                       | BasicUnboundBool Bool
                       | BasicUnboundNA

instance Entry BasicUnboundEntry where
  readEntry val
    |isJust (readMaybe val :: Maybe Integer) =
       BasicUnboundInt $ read val
    |isJust (readMaybe val :: Maybe Double) =
       BasicUnboundDouble $ read val
    |isJust (readMaybe val :: Maybe Bool) =
       BasicUnboundBool $ read val
    |val == "NA" = BasicUnboundNA
    |otherwise = BasicUnboundString $ Text.pack val

  getEntry (BasicUnboundInt val) = cast val
  getEntry (BasicUnboundDouble val) = cast val
  getEntry (BasicUnboundRatio val) = cast val
  getEntry (BasicUnboundString val) = cast val
  getEntry (BasicUnboundBool val) = cast val
  getEntry _ = Nothing

  isEntryNA BasicUnboundNA = True
  isEntryNA _ = False

  na = BasicUnboundNA

  summary vs
    |V.null vs = "Empty vector"
    |not (null (getEntries vs :: [Integer])) = let vs' = (getEntries vs :: [Integer]) in
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
    |not (null (getEntries vs :: [Float])) =
       let vs' = (getEntries vs :: [Float]) in
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
    |not (null (getEntries vs :: [Ratio Integer])) =
       let vs' = (getEntries vs :: [Ratio Integer]) in
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
