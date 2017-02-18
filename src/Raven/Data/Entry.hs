module Raven.Data.Entry
  ( Entry
  , readEntry
  , getEntry
  , isEntryNA
  , na
  , summary
  , getEntries
  , countNAs
  ) where

import Data.Vector(Vector)
import qualified Data.Vector as V
import Data.Typeable
import Data.Text (Text)

-- |Entry is a polymorphic type used by tables.
-- This allows tables to have a single entry type (and be more flexible)
class Entry a where
  -- |Reads an entry from a string.
  -- Note that this does not implement read,
  -- so completed entries can be stored and read later.
  readEntry :: String -> a
  -- |Pulls a number from an entry value (if possible)
  getEntry :: (Typeable b) => a -> Maybe b
  -- |Tests if the entry is NA
  isEntryNA :: a -> Bool
  -- |Returns an NA
  na :: a
  -- |Returns a formatted summary of the data type pulled from a vector
  summary :: Vector a -> Text

-- |Pulls the entry into a vector of a specific type, ignoring NA's and using getEnrty
getEntries :: (Entry a,Typeable b) => Vector a -> [b]
getEntries = reverse . V.foldl' (\acc val -> case getEntry val of
                                               Just val' -> val':acc
                                               Nothing -> acc) []

-- |Count the number of NA's in a Vector of entries
countNAs :: (Entry a) => Vector a -> Int
countNAs = V.foldl' count 0
  where
    count acc val
      |isEntryNA val = succ acc
      |otherwise = acc
