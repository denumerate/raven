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
  MVar (Map ConnectionId Connection) -> ProcessId -> IO ()
listenAtEnd trans end serverN conns pid = receive end >>=
  (\event -> case event of
      ConnectionOpened cid reliabilty adrs -> forkIO
             (takeMVar conns >>=
              (\conns' ->
                  Network.Transport.connect end adrs reliabilty
                  (defaultConnectHints{connectTimeout = Just connTimeout}) >>=
                  (\conn -> case conn of
                      Right conn' -> putMVar conns $ Map.insert cid conn' conns'
                      Left err ->
                        putMVar conns conns' >>
                        runProcess serverN
                            (buildLogMsg
                              ("Connection failed with " ++ show adrs
                                ++ ". " ++ show err) >>=
                              Control.Distributed.Process.send pid)))) >>
                listenAtEnd trans end serverN conns pid
      Received cid [info] -> forkIO
             (let wds = B.words info in
                 runProcess serverN (handleReceived pid wds cid)) >>
             listenAtEnd trans end serverN conns pid
      ConnectionClosed cid ->
        takeMVar conns >>=
        putMVar conns . Map.delete cid >>
        runProcess serverN
             (Control.Distributed.Process.send pid (ConnClosed cid)) >>
        listenAtEnd trans end serverN conns pid
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
sendResult :: MVar (Map ConnectionId Connection) -> MVar ProcessId ->
  (ConnectionId,ProcessedMsg) -> Process ()
sendResult conns server (cid,ProcessedMsg n msg) =
  liftIO (readMVar conns) >>=
  (\conns' ->
     liftIO (readMVar server) >>=
     (\server' ->
        case Map.lookup cid conns' of
          Just conn ->
            liftIO (Network.Transport.send conn [n," ",B.pack msg]) >>=
            handleSent server'
          _ ->
            buildLogMsg "Connection not found on send" >>=
            Control.Distributed.Process.send server'))
sendResult conns server (cid,ProcessedBSMsg n msg) =
  liftIO (readMVar conns) >>=
  (\conns' ->
     liftIO (readMVar server) >>=
     (\server' ->
        case Map.lookup cid conns' of
          Just conn ->
            liftIO (Network.Transport.send conn [n," ",msg]) >>=
            handleSent server'
          _ ->
            buildLogMsg "Connection not found on send" >>=
            Control.Distributed.Process.send server'))

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
handleReceived pid msg _ = buildLogMsg
   ("Received, not recognized message from outside connection: " ++ show msg) >>=
   Control.Distributed.Process.send pid
