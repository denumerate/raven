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
import Crypto.Hash

import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.Text as Text
import Data.Text(Text)

import Raven.Server.NodeMsgs

-- |Stores the node and the id of the listen process
type ConnNode = (LocalNode,MVar ProcessId)

-- |Stores information about the user on that connection (Token,Id)
type User = Maybe (Text,Text)

-- |Builds and returns node to handle connections.
-- Needs the transport layer, server id, and the established connection
newConnNode :: Transport -> ProcessId -> Connection -> IO ConnNode
newConnNode trans server conn = newEmptyMVar >>=
  (\pid -> newMVar Nothing >>=
    (\cInfo -> newLocalNode trans initRemoteTable >>=
      (\connNode ->
         runProcess connNode
        (spawnLocal (forever (receiveWait
                              [ match (sendResult conn)
                              , match (handleLog server)
                              , match (handleKill conn)
                              , match (handleNewToken cInfo)
                              , matchUnknown (catchAllMsgs server "ConnNode")
                              ])) >>=
         liftIO . putMVar pid) >>
        return (connNode,pid))))

-- |Takes the result of the servers work and sends it back to the client
sendResult :: Connection -> ProcessedMsg -> Process ()
sendResult conn (ProcessedMsg n msg) =
  liftIO (Network.Transport.send conn [n," ",B.pack msg]) >> --error possible here
  return ()

-- |Handles the data send by a received event
-- needs the servers process id
handleReceived :: ProcessId -> [ByteString] -> ConnNode -> IO ()
handleReceived pid [n,":kill"] (connNode,self) = runProcess connNode
  (liftIO (readMVar self) >>=
   (\self' -> Control.Distributed.Process.send pid (self',KillMsg n)))
handleReceived pid [n,":logon",name,pass] (connNode,self) = forkIO
  (readMVar self >>=
   (\self' -> runProcess connNode
     (Control.Distributed.Process.send pid
      (self',LoginMsg n (Text.pack (B.unpack name))
        (Text.pack (show (hash pass :: Digest SHA3_512))))))) >>
  return ()
handleReceived pid [n,":logout"] (connNode,self) = runProcess connNode
  (liftIO (readMVar self) >>=
   (\self' -> Control.Distributed.Process.send pid (self', LogoutMsg n)))
handleReceived pid (n:msg) (connNode,self) = readMVar self >>=
  (\self' -> runProcess connNode
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

-- |Handles a NewToken message by updating the user information.
handleNewToken :: MVar User -> NewTokenMsg -> Process ()
handleNewToken u (NewTokenMsg ui) =
  liftIO (takeMVar u) >>
  liftIO (putMVar u (Just ui))
