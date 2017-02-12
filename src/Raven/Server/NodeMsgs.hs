{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Raven.Server.NodeMsgs
  ( TestMsg (..)
  ) where

import Data.Binary
import Data.Typeable
import Data.ByteString.Char8 (ByteString)


newtype TestMsg = TestMsg [ByteString]
  deriving (Binary,Typeable)
