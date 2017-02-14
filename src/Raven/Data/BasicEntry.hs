module Raven.Data.BasicEntry
  ( BasicEntry (..)
  )where

import Data.Ratio
import qualified Data.Text as Text
import Data.Text (Text)
import Data.Typeable
import Text.Read

import Raven.Data.Entry

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
