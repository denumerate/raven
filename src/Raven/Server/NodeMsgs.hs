{-# LANGUAGE
  DeriveAnyClass
, DeriveGeneric
#-}
module Raven.Server.NodeMsgs
  ( REPLMsg(..)
  , ProcessedMsg(..)
  , KillMsg(..)
  , LogMsg(..)
  , buildLogMsg
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

-- |Standardized LogMsg
buildLogMsg :: String -> Process LogMsg
buildLogMsg msg = getSelfPid >>=
  (\self -> liftIO getCurrentTime >>=
    (\time -> return (LogMsg msg self (show time))))

-- |Final check to pick up unmatched messages.
-- Needs the pid of the listen process, and node type
catchAllMsgs :: ProcessId -> String -> Process ()
catchAllMsgs pid node = buildLogMsg (node ++ " did not match a message") >>=
  Control.Distributed.Process.send pid

-- |CatchAllMsgs with MVar
catchAllMsgs' :: MVar ProcessId -> String -> Process ()
catchAllMsgs' pid node = liftIO (readMVar pid) >>=
  (`catchAllMsgs` node)
