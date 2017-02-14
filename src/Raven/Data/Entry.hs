module Raven.Data.Entry
  ( Entry
  , readEntry
  , getEntries
  , getEntry
  , isEntryNA
  , na
  ) where

import Data.Vector(Vector)
import qualified Data.Vector as V
import Data.Typeable

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


-- |Pulls the entry into a vector of a specific type, ignoring NA's and using getEnrty
getEntries :: (Entry a,Typeable b) => Vector a -> [b]
getEntries = reverse . V.foldl' (\acc val -> case getEntry val of
                                               Just val' -> val':acc
                                               Nothing -> acc) []
