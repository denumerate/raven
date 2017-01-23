module Raven.Data.Entry
  ( Entry
  , buildEntry
  , dumpEntry
  , isNA
  , BasicEntry) where

import Data.Ratio
import qualified Data.Text as Text
import Data.Text (Text)
import Data.Typeable

-- |Creates the requirements for an Entry which are:
-- the entry must be able to go from a typeable input and back through
-- buildEntry, which checks the input against possible values and declares them,
-- and dumpEntry, which dumps the values
-- and there must be an NA value to indicate missing or corrupted data
-- and can be checked for with the isNA predicate
class Entry a where
  buildEntry :: (Typeable b) => b -> a
  dumpEntry :: (Typeable b) => a -> b
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
       Just val' -> BasicDouble val'
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

-- | BasicUnboundEntry creates a simple entry with unbounded values
data BasicUnboundEntry = BasicUnboundInt Integer
                       | BasicUnboundDouble Double
                       | BasicUnboundRatio Rational
                       | BasicUnboundString Text
                       | BasicUnboundBool Bool
                       | BasicUnboundNA
