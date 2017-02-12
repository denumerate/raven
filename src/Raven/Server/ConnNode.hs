module Raven.Server.ConnNode
  ( ConnNode
  , newConnNode
  , handleReceived
  )where

import Network.Transport
import Control.Distributed.Process
import Control.Distributed.Process.Node
import Control.Concurrent
import Control.Monad (forever)

import Data.ByteString.Char8 (ByteString)

import Raven.Server.NodeMsgs

-- |Stores the node and the id of the listen process
type ConnNode = (LocalNode,MVar ProcessId)

-- |Builds and returns node to handle connections.
-- Needs the transport layer, server node's pid, and the established connection
newConnNode :: Transport -> Connection -> IO ConnNode
newConnNode trans conn = newEmptyMVar >>=
  (\pid -> newLocalNode trans initRemoteTable >>=
    (\connNode ->
       runProcess connNode
       (spawnLocal (forever (receiveWait [match (sendResult conn)])) >>=
        liftIO . putMVar pid) >>
       return (connNode,pid)))

-- |Takes the result of the servers work and sends it back to the client
sendResult :: Connection -> TestMsg -> Process ()
sendResult conn (TestMsg msg) =
  liftIO $ Network.Transport.send conn msg >>
  return ()

-- |Handles the data send by a received event
-- needs the servers process id
handleReceived :: MVar ProcessId -> [ByteString] -> ConnNode -> IO ()
handleReceived pid received (connNode,self) = readMVar pid >>=
  (\pid' -> readMVar self >>=
    (\self' -> runProcess connNode
      (Control.Distributed.Process.send pid' (self',TestMsg received))))
