{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Raven.Server.NodeMsgs
  ( REPLMsg(..)
  , ProcessedMsg(..)
  , KillMsg(..)
  ) where

import Data.Binary
import Data.Typeable
import Data.Text (Text)

newtype REPLMsg = REPLMsg String
  deriving (Binary,Typeable)

newtype ProcessedMsg = ProcessedMsg String
  deriving (Binary,Typeable)

newtype KillMsg = KillMsg Text
  deriving (Binary,Typeable)
