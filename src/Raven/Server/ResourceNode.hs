module Raven.Server.ResourceNode
  ( LogLevel(..)
  , ResourceNode
  , buildResourceNode
  , cleanResourceNode
  )where

import Network.Transport
import Control.Distributed.Process
import Control.Distributed.Process.Node
import Control.Concurrent
import Control.Monad(forever)

-- |Indicates to what extent events are logged
data LogLevel = None
              | Standard
              | Verbose

-- |Stores the ProcessId of the listen process
type ResourceNode = MVar ProcessId

-- |Builds and returns the node.
-- Needs the transport layer and Maybe a logging level (Standard is default)
buildResourceNode :: Transport -> Maybe LogLevel -> IO ResourceNode
buildResourceNode trans logLvl =
  (case logLvl of
      Just lvl -> newMVar lvl
      _ -> newMVar Standard) >>=
  (\logLvl' -> newLocalNode trans initRemoteTable >>=
    (\node -> newEmptyMVar >>=
      (\pid -> runProcess node
               (spawnLocal (forever (receiveWait [])) >>=
                liftIO . putMVar pid) >>
               return pid)))

-- |Tells the listening process on a resourceNode to exit
cleanResourceNode :: ResourceNode -> Process ()
cleanResourceNode self = liftIO (readMVar self) >>=
  (`exit` "Cleaning ResourceNode")
