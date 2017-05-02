{-# LANGUAGE OverloadedStrings #-}
module Raven.Server.Connection
  ( listenAtEnd
  , handleReceived
  , sendResult
  )where

import Network.Transport
import Control.Distributed.Process
import Control.Distributed.Process.Node
import Control.Concurrent

import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.Map (Map)
import qualified Data.Map as Map

import Raven.Server.NodeMsgs
import Raven.Server.Commands

-- |This is a guess as to what the value should be, documentation is unclear.
connTimeout :: Int
connTimeout = 1800000000

-- |Listen for and handle events from the endpoint.
-- The pid is the id of the node's listening process
listenAtEnd :: Transport -> EndPoint -> LocalNode ->
  Map ConnectionId (MVar Connection) -> ProcessId -> IO ()
listenAtEnd trans end serverN conns pid = receive end >>=
  (\event -> case event of
      ConnectionOpened cid reliabilty adrs -> newEmptyMVar >>=
             (\cNode -> forkIO
               (Network.Transport.connect end adrs reliabilty
                (defaultConnectHints{connectTimeout = Just connTimeout}) >>=
                 (\conn -> case conn of
                     Right conn' -> putMVar cNode conn'
                     Left err ->
                       runProcess serverN
                           (buildLogMsg
                             ("Connection failed with " ++ show adrs
                               ++ ". " ++ show err) >>=
                             Control.Distributed.Process.send pid))) >>
               listenAtEnd trans end serverN (Map.insert cid cNode conns) pid)
      Received cid [info] -> forkIO
             (let wds = B.words info in
                 runProcess serverN (handleReceived pid wds cid)) >>
             listenAtEnd trans end serverN conns pid
      ConnectionClosed cid -> listenAtEnd trans end serverN
                              (Map.delete cid conns) pid
      EndPointClosed -> putStrLn "EndPoint Closed"
      ErrorEvent (TransportError _ err) ->
        runProcess serverN
          (buildLogMsg ("EndPoint Error: " ++ err) >>=
            Control.Distributed.Process.send pid) >>
             listenAtEnd trans end serverN conns pid
      _ -> forkIO (runProcess serverN
                   (buildLogMsg "Uncaught event at endpoint" >>=
                    Control.Distributed.Process.send pid)) >>
           listenAtEnd trans end serverN conns pid)

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
handleReceived :: ProcessId -> [ByteString] -> ConnectionId -> Process ()
handleReceived _ [_] _ = return ()
handleReceived pid cmd@(n:msg) self =
  if B.isPrefixOf ":" (head msg)
    then parseCommand pid self cmd
    else Control.Distributed.Process.send pid (self,REPLMsg n
                                                (B.unpack (B.unwords msg)))
handleReceived pid msg _ =buildLogMsg
   ("Received, not recognized message from outside connection: " ++ show msg) >>=
   Control.Distributed.Process.send pid
