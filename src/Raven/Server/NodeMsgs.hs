{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Raven.Server.NodeMsgs
  ( REPLMsg(..)
  , ProcessedMsg(..)
  ) where

import Data.Binary
import Data.Typeable

newtype REPLMsg = REPLMsg String
  deriving (Binary,Typeable)

newtype ProcessedMsg = ProcessedMsg String
  deriving (Binary,Typeable)
