module Raven.Data.Entry
  ( Entry
  , buildEntry
  , getEntry
  , isEntryNA
  , getEntryVector
  , BasicEntry
  , BasicUnboundEntry
  ) where

import Data.Ratio
import qualified Data.Text as Text
import Data.Text (Text)
import Data.Vector(Vector)
import qualified Data.Vector as V
import Data.Typeable
import GHC.Float

-- |Entry is a polymorphic type used by tables.
-- This allows tables to have a single entry type (and be more flexible)
class Entry a where
  -- |build entry takes any type and turns it into an entry
  buildEntry :: (Typeable b) => b -> a
  -- |Pulls a number from an entry value (if possible)
  getEntry :: (Typeable b) => a -> Maybe b
  -- |Tests if the entry is NA
  isEntryNA :: a -> Bool

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

  getEntry (BasicInt val) = cast val
  getEntry (BasicDouble val) = cast val
  getEntry (BasicRatio val) = cast val
  getEntry (BasicString val) = cast val
  getEntry (BasicBool val) = cast val
  getEntry _ = Nothing

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

  getEntry (BasicUnboundInt val) = cast val
  getEntry (BasicUnboundDouble val) = cast val
  getEntry (BasicUnboundRatio val) = cast val
  getEntry (BasicUnboundString val) = cast val
  getEntry (BasicUnboundBool val) = cast val
  getEntry _ = Nothing

  isEntryNA BasicUnboundNA = True
  isEntryNA _ = False
