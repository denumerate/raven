module Raven.Data.Entry () where

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
    |

-- | BasicUnboundEntry creates a simple entry with unbounded values
data BasicUnboundEntry = BasicUnboundInt Integer
                       | BasicUnboundDouble Double
                       | BasicUnboundRatio (Ratio Integer)
                       | BasicUnboundString Text
                       | BasicUnboundBool Bool
                       | BasicUnboundNA
