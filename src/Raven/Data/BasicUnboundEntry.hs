module Raven.Data.BasicUnboundEntry
  ( BasicUnboundEntry(..)
  )where

import Data.Text (Text)
import qualified Data.Text as Text
import Text.Read
import Data.Typeable

import Raven.Data.Entry

-- | BasicUnboundEntry creates a simple entry with unbounded values
data BasicUnboundEntry = BasicUnboundInt Integer
                       | BasicUnboundDouble Double
                       | BasicUnboundRatio Rational
                       | BasicUnboundString Text
                       | BasicUnboundBool Bool
                       | BasicUnboundNA

instance Entry BasicUnboundEntry where
  readEntry val
    |(readMaybe val :: Maybe Integer) /= Nothing =
       BasicUnboundInt $ read val
    |(readMaybe val :: Maybe Double) /= Nothing =
       BasicUnboundDouble $ read val
    |(readMaybe val :: Maybe Bool) /= Nothing =
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
