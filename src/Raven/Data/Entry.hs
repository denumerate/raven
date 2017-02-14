module Raven.Data.Entry
  ( Entry
  , readEntry
  , getEntry
  , isEntryNA
  , na
  , getEntryVector
  , BasicEntry
  , BasicUnboundEntry
  ) where

import Data.Ratio
import qualified Data.Text as Text
import Data.Text (Text)
import Text.Read
import Data.Vector(Vector)
import qualified Data.Vector as V
import Data.Typeable

-- |Entry is a polymorphic type used by tables.
-- This allows tables to have a single entry type (and be more flexible)
class Entry a where
  -- |Reads an entry from a string
  readEntry :: String -> a
  -- |Pulls a number from an entry value (if possible)
  getEntry :: (Typeable b) => a -> Maybe b
  -- |Tests if the entry is NA
  isEntryNA :: a -> Bool
  -- |Returns an NA
  na :: a

-- |BasicEntry creates a simple entry with bounded values
data BasicEntry = BasicInt Int
                | BasicDouble Double
                | BasicRatio (Ratio Int)
                | BasicString Text
                | BasicBool Bool
                | BasicNA

-- |Pulls the entry into a vector of a specific type, ignoring NA's and using getEnrty
getEntryVector :: (Entry a,Typeable b) =>  Vector a -> Vector b
getEntryVector = V.fromList . reverse . V.foldl' (\acc val -> case getEntry val of
                                                     Just val' -> val':acc
                                                     Nothing -> acc) []

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
