{-# LANGUAGE OverloadedStrings #-}
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
import Raven.Server.Commands

-- |Stores the node and the id of the listen process
type ConnNode = (LocalNode,MVar ProcessId)

-- |Builds and returns node to handle connections.
-- Needs the transport layer, server id, and the established connection
newConnNode :: Transport -> ProcessId -> Connection -> IO ConnNode
newConnNode trans server conn = newEmptyMVar >>=
  (\pid -> newLocalNode trans initRemoteTable >>=
    (\connNode ->
       runProcess connNode
       (spawnLocal (forever (receiveWait
                              [ match (sendResult conn server)
                              , match (handleLog server)
                              , match (handleKill conn)
                              , matchUnknown (catchAllMsgs server "ConnNode")
                              ])) >>=
         liftIO . putMVar pid) >>
        return (connNode,pid)))

-- |Takes the result of the servers work and sends it back to the client
sendResult :: Connection -> ProcessId -> ProcessedMsg -> Process ()
sendResult conn server (ProcessedMsg n msg) =
  liftIO (Network.Transport.send conn [n," ",B.pack msg]) >>= --error possible here
  handleSent server
sendResult conn server (ProcessedBSMsg n msg) =
  liftIO (print (B.length msg)) >>
  liftIO (Network.Transport.send conn [n," ",msg]) >>= --error possible here
  handleSent server

-- |handles the output from a Transport send
handleSent :: ProcessId -> Either (TransportError SendErrorCode) () -> Process ()
handleSent server result = case result of
  Left err -> buildLogMsg (show err) >>=
    Control.Distributed.Process.send server
  _ -> return ()

-- |Handles the data send by a received event
-- needs the servers process id
handleReceived :: ProcessId -> [ByteString] -> ConnNode -> IO ()
handleReceived _ [_] _ = return ()
handleReceived pid cmd@(n:msg) (connNode,self) = readMVar self >>=
  (\self' -> if B.isPrefixOf ":" (head msg)
    then runProcess connNode (parseCommand pid self' cmd)
    else runProcess connNode
         (Control.Distributed.Process.send pid (self',REPLMsg n
                                                 (B.unpack (B.unwords msg)))))
handleReceived pid msg (connNode,_) = runProcess connNode
  (buildLogMsg
   ("Received, not recognized message from outside connection: " ++ show msg) >>=
   Control.Distributed.Process.send pid)

-- |Handle a kill message by killing the node
handleKill :: Connection -> KillMsg -> Process ()
handleKill conn _ = liftIO (close conn) >>
  getSelfPid >>= (`exit` ("Clean" :: ByteString))

-- |Tells the listening process on a connNode to exit
cleanConnNode :: ConnNode -> IO ()
cleanConnNode (connNode,self) = readMVar self >>=
  (\self' -> runProcess connNode
    (Control.Distributed.Process.send self' (KillMsg "")))

-- |Handle a LogMsg by sending it to the supplied processId.
-- Process should be the server node.
handleLog :: ProcessId -> LogMsg -> Process ()
handleLog = Control.Distributed.Process.send
