module Raven.Data.Entry () where

import Data.Ratio
import qualified Data.Text as Text
import Data.Text (Text)

class Entry a where
  buildEntry :: b -> a
  dumpEntry :: a -> b

data BasicEntry = BasicInt Int
                | BasicDouble Double
                | BasicRatio (Ratio Int)
                | BasicString Text
                | BasicBool Bool
                | BasicNA

instance Entry BasicEntry where
  

data BasicUnboundEntry = BasicUnboundInt Integer
                       | BasicUnboundDouble Double
                       | BasicUnboundRatio (Ratio Integer)
                       | BasicUnboundString Text
                       | BasicUnboundBool Bool
                       | BasicUnboundNA
