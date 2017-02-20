module Raven.Server.ConnNode
  ( ConnNode
  , newConnNode
  , handleReceived
  , cleanConnNode
  )where

import Network.Transport
import Control.Distributed.Process
import Control.Distributed.Process.Node
import Control.Concurrent
import Control.Monad (forever)

import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B

import Raven.Server.NodeMsgs

-- |Stores the node and the id of the listen process
type ConnNode = (LocalNode,MVar ProcessId)

-- |Builds and returns node to handle connections.
-- Needs the transport layer, and the established connection
newConnNode :: Transport -> Connection -> IO ConnNode
newConnNode trans conn = newEmptyMVar >>=
  (\pid -> newLocalNode trans initRemoteTable >>=
    (\connNode ->
       runProcess connNode
       (spawnLocal (forever (receiveWait [match (sendResult conn)])) >>=
        liftIO . putMVar pid) >>
       return (connNode,pid)))

-- |Takes the result of the servers work and sends it back to the client
sendResult :: Connection -> ProcessedMsg -> Process ()
sendResult conn (ProcessedMsg msg) =
  liftIO $ Network.Transport.send conn [B.pack msg] >>
  return ()

-- |Handles the data send by a received event
-- needs the servers process id
handleReceived :: ProcessId -> [ByteString] -> ConnNode -> IO ()
handleReceived pid [msg] (connNode,self) = readMVar self >>=
  (\self' -> runProcess connNode
    (Control.Distributed.Process.send pid (self',REPLMsg (B.unpack msg))))

-- |Tells the listening process on a connNode to exit
cleanConnNode :: ConnNode -> IO ()
cleanConnNode (connNode,self) = readMVar self >>=
  (\self' -> runProcess connNode
    (exit self' "Cleaning ConnNode")) --log?
