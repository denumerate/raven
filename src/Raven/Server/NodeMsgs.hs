{-# LANGUAGE
  DeriveAnyClass
, DeriveGeneric
#-}
module Raven.Server.NodeMsgs
  ( REPLMsg(..)
  , ProcessedMsg(..)
  , KillMsg(..)
  , LogMsg(..)
  , catchAllMsgs
  , catchAllMsgs'
  ) where

import Control.Distributed.Process
import Control.Concurrent

import Data.Binary
import Data.Typeable
import Data.Time.Clock
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

-- |Final check to pick up unmatched messages.
-- Needs the pid of the listen process, and node type
catchAllMsgs :: ProcessId -> String -> Process ()
catchAllMsgs pid node = getSelfPid >>=
  (\self -> liftIO getCurrentTime >>=
    (\time -> pid `Control.Distributed.Process.send`
       (LogMsg (node ++ " did not match a message") self (show time))))

-- |CatchAllMsgs with MVar
catchAllMsgs' :: MVar ProcessId -> String -> Process ()
catchAllMsgs' pid node = liftIO (readMVar pid) >>=
  (`catchAllMsgs` node)
