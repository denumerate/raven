{-# LANGUAGE
  DeriveAnyClass
, DeriveGeneric
#-}
module Raven.Server.NodeMsgs
  ( REPLMsg(..)
  , ProcessedMsg(..)
  , KillMsg(..)
  , LogMsg(..)
  ) where

import Control.Distributed.Process

import Data.Binary
import Data.Typeable
import GHC.Generics

data REPLMsg = REPLMsg String
  deriving (Generic,Binary,Typeable)

data ProcessedMsg = ProcessedMsg String
  deriving (Generic,Binary,Typeable)

data KillMsg = KillMsg
  deriving (Generic,Binary,Typeable)

-- |LogMsg holds: the message, processId, timestamp
data LogMsg = LogMsg String ProcessId String
  deriving (Generic,Binary,Typeable)
