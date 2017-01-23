module Raven.Data.Entry () where

import Data.Ratio
import qualified Data.Text as Text
import Data.Text (Text)
import Data.Typeable

class Entry a where
  buildEntry :: (Typeable b) => b -> a
  dumpEntry :: (Typeable b) => a -> b

data BasicEntry = BasicInt Int
                | BasicDouble Double
                | BasicRatio (Ratio Int)
                | BasicString Text
                | BasicBool Bool
                | BasicNA

instance Entry BasicEntry where
  buildEntry val
    |

data BasicUnboundEntry = BasicUnboundInt Integer
                       | BasicUnboundDouble Double
                       | BasicUnboundRatio (Ratio Integer)
                       | BasicUnboundString Text
                       | BasicUnboundBool Bool
                       | BasicUnboundNA
