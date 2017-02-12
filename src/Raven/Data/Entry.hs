module Raven.Data.Entry
  ( Entry
  , buildEntry
  , dumpEntry
  , isNA
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
  -- |Gets an Integral from an entry (if possible)
  getIntegral :: (Integral b) => a -> Maybe b
  -- |Gets a floating from an entry (if possible)
  getFloating :: (Floating b) => a -> Maybe b
  -- |Gets a ratio from an entry (if possible)
  getRatio :: (Integral b) => a -> Maybe (Ratio b)
  -- |Gets a Text from an entry (if possible)
  getText :: a -> Maybe Text
  -- |Gets a Bool from an entry (if possible)
  getBool :: a -> Maybe Bool
  -- |Simply prints the entry, if not NA
  getEntry :: a -> Maybe Text
  isNA :: a -> Bool

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

  getIntegral (BasicInt val) = Nothing
  getIntegral (BasicDouble val) = Nothing
  getIntegral (BasicRatio val) = Nothing
  getIntegral (BasicString val) = Nothing
  getIntegral (BasicBool val) = Nothing
  getIntegral BasicNA = Nothing

  isNA BasicNA = True
  isNA _ = False

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

  getIntegral (BasicUnboundInt val) = Just val
  getIntegral (BasicUnboundDouble val) = Nothing
  getIntegral (BasicUnboundRatio val) = Nothing
  getIntegral (BasicUnboundString val) = Nothing
  getIntegral (BasicUnboundBool val) = Nothing
  getIntegral BasicUnboundNA = Nothing

  isNA BasicUnboundNA = True
  isNA _ = False
