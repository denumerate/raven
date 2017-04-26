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
import Control.Monad (forever,void)

import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as LB

import System.Directory
import Codec.Picture

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
                              , match (handlePlotDone conn server)
                              , match (handleLog server)
                              , match (handleKill conn)
                              , matchUnknown (catchAllMsgs server "ConnNode")
                              ])) >>=
         liftIO . putMVar pid) >>
        return (connNode,pid)))

-- |Takes the result of the servers work and sends it back to the client
sendResult :: Connection -> ProcessId -> ProcessedMsg -> Process ()
sendResult conn server (ProcessedMsg n msg) =
  liftIO (Network.Transport.send conn [n," ",B.pack msg]) >>=
  handleSent server
sendResult conn server (ProcessedBSMsg n msg) =
  liftIO (Network.Transport.send conn [n," ",msg]) >>=
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

-- |Handles a PlotDoneMsg by sending a serialized version of the image to the user
-- and then removing the file
handlePlotDone :: Connection -> ProcessId -> PlotDoneMsg -> Process ()
handlePlotDone conn server (PlotDoneMsg n fname) =
  let fname' = ".raven/plots/" ++ fname
  in void $ spawnLocal
     (liftIO (doesFileExist fname') >>=
       (\exists -> if exists then liftIO (readImage fname') >>=
         (\img -> case img of
             Left str ->
               liftIO (Network.Transport.send conn [n," ",B.pack str]) >>=
               handleSent server
             Right img' -> case encodeDynamicBitmap img' of
               Left str ->
                 liftIO (Network.Transport.send conn [n," ",B.pack str]) >>=
                 handleSent server
               Right bstring ->
                 liftIO (Network.Transport.send conn [n," ",LB.toStrict bstring]) >>=
                 handleSent server) >>
         liftIO (removeFile fname')
                   else
                     liftIO (Network.Transport.send conn [n," ","File Error"]) >>=
                     handleSent server))
