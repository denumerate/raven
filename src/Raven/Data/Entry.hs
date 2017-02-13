module Raven.Data.Entry
  ( Entry
  , buildEntry
  , getEntryNum
  , isEntryNA
  , BasicEntry
  , BasicUnboundEntry
  ) where

import Data.Ratio
import qualified Data.Text as Text
import Data.Text (Text)
import Data.Typeable
import GHC.Float

-- |Entry is a polymorphic type used by tables.
-- This allows tables to have a single entry type (and be more flexible)
class Entry a where
  -- |build entry takes any type and turns it into an entry
  buildEntry :: (Typeable b) => b -> a
  -- |Pulls a number from an entry value (if possible)
  getEntryNum :: (Num b) => a -> Maybe b
  -- |Tests if the entry is NA
  isEntryNA :: a -> Bool

-- |BasicEntry creates a simple entry with bounded values
data BasicEntry = BasicInt Int
                | BasicDouble Double
                | BasicRatio (Ratio Int)
                | BasicString Text
                | BasicBool Bool
                | BasicNA

instance Entry BasicEntry where
  buildEntry val
    |typeOf val == typeOf (4 :: Int) = case cast val of
       Just val' -> BasicInt val'
       _ -> BasicNA
    |typeOf val == typeOf (4 :: Integer) = case cast val of
       Just val' -> BasicInt $ fromInteger val'
       _ -> BasicNA
    |typeOf val == typeOf (4 :: Float) = case cast val of
       Just val' -> BasicDouble $ float2Double val'
       _ -> BasicNA
    |typeOf val == typeOf (4 :: Double) = case cast val of
       Just val' -> BasicDouble val'
       _ -> BasicNA
    |typeOf val == typeOf ((4 :: Int) % (4 :: Int)) = case cast val of
       Just val' -> BasicRatio $ (fromInteger . numerator) val' %
         (fromInteger . denominator) val'
       _ -> BasicNA
    |typeOf val == typeOf ((4 :: Integer) % (4 :: Integer)) = case cast val of
       Just val' -> BasicRatio val'
       _ -> BasicNA
    |typeOf val == typeOf "check" = case cast val of
       Just val' -> BasicString $ Text.pack val'
       _ -> BasicNA
    |typeOf val == typeOf Text.empty = case cast val of
       Just val' -> BasicString val'
       _ -> BasicNA
    |typeOf val == typeOf True = case cast val of
       Just val' -> BasicBool val'
       _ -> BasicNA
    |otherwise = BasicNA

  getEntryNum (BasicInt val) = Just $ fromIntegral val
  getEntryNum (BasicDouble val) = cast val
  getEntryNum (BasicRatio val) = Just val
  getEntryNum _ = Nothing

  isEntryNA BasicNA = True
  isEntryNA _ = False

-- | BasicUnboundEntry creates a simple entry with unbounded values
data BasicUnboundEntry = BasicUnboundInt Integer
                       | BasicUnboundDouble Double
                       | BasicUnboundRatio Rational
                       | BasicUnboundString Text
                       | BasicUnboundBool Bool
                       | BasicUnboundNA

instance Entry BasicUnboundEntry where
  buildEntry val
    |typeOf val == typeOf (4 :: Int) = case (cast val :: Maybe Int) of
       Just val' -> BasicUnboundInt $ fromIntegral val'
       _ -> BasicUnboundNA
    |typeOf val == typeOf (4 :: Integer) = case cast val of
       Just val' -> BasicUnboundInt  val'
       _ -> BasicUnboundNA
    |typeOf val == typeOf (4 :: Float) = case cast val of
       Just val' -> BasicUnboundDouble $ float2Double val'
       _ -> BasicUnboundNA
    |typeOf val == typeOf (4 :: Double) = case cast val of
       Just val' -> BasicUnboundDouble val'
       _ -> BasicUnboundNA
    |typeOf val == typeOf ((4 :: Int) % (4 :: Int)) =
       case (cast val :: Maybe (Ratio Int)) of
         Just val' -> BasicUnboundRatio $ (fromIntegral . numerator) val' %
           (fromIntegral . denominator) val'
         _ -> BasicUnboundNA
    |typeOf val == typeOf ((4 :: Integer) % (4 :: Integer)) = case cast val of
       Just val' -> BasicUnboundRatio val'
       _ -> BasicUnboundNA
    |typeOf val == typeOf "check" = case cast val of
       Just val' -> BasicUnboundString $ Text.pack val'
       _ -> BasicUnboundNA
    |typeOf val == typeOf Text.empty = case cast val of
       Just val' -> BasicUnboundString val'
       _ -> BasicUnboundNA
    |typeOf val == typeOf True = case cast val of
       Just val' -> BasicUnboundBool val'
       _ -> BasicUnboundNA
    |otherwise = BasicUnboundNA

  isEntryNA BasicUnboundNA = True
  isEntryNA _ = False
