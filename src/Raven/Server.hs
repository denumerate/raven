{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Raven.Server
  ( initServer
  ) where

import Network
import Network.Transport
import Network.Transport.TCP
import Control.Distributed.Process
import Control.Distributed.Process.Node
import Control.Concurrent
import Control.Monad (forever,unless)

import Data.Map (Map)
import qualified Data.Map as Map
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.Binary
import Data.Typeable

import Raven.REPL

newtype TestMsg = TestMsg [ByteString]
  deriving (Binary,Typeable)

-- |Start the server and listen for connections at the supplied ip:port number.
-- Returns the address of the server endpoint (if successful)
-- All async
initServer :: String -> String -> IO ()
initServer ip portNum = withSocketsDo $
  createTransport ip portNum defaultTCPParameters >>=
  (\trans -> case trans of
      Right trans' -> newEndPoint trans' >>=
        (\end -> case end of
            Right end' -> newLocalNode trans' initRemoteTable >>=
              (\node -> putStrLn ("Server established at " ++ (show . address) end') >>
                newEmptyMVar >>=
                (\pid ->
                    runProcess node
                    (spawnLocal (liftIO (listenAtEnd trans' pid end' Map.empty)) >>
                     spawnLocal (forever (receiveWait [match handleTest])) >>=
                     liftIO . putMVar pid>>
                     unless True (return ()))))
            _ -> putStrLn "Endpoint not initialized, Server Failed" >> --move to log
                 return ())
      _ -> putStrLn "Transport not initialized, Server Failed" >> --move to log
        return ())

-- |Listen for and handle events from the endpoint.
-- The pid is the id of the node's listening process
listenAtEnd :: Transport -> MVar ProcessId -> EndPoint ->
  Map ConnectionId (MVar Connection) -> IO ()
listenAtEnd trans pid end conns = receive end >>=
  (\event -> case event of
      ConnectionOpened cid reliabilty adrs -> newEmptyMVar >>=
        (\cEntry -> forkIO
          (Network.Transport.connect end adrs reliabilty defaultConnectHints >>=
           (\conn -> case conn of
               Right conn' -> putMVar cEntry conn' >>
                 return ()
               _ -> putStrLn "Connection failed")) >> --move to log
          listenAtEnd trans pid end (Map.insert cid cEntry conns))
      Received cid info -> forkIO
        (case Map.lookup cid conns of
            Just conn -> readMVar conn >>=
              handleData trans pid info >>
              return ()
            Nothing -> putStrLn "Connection not found") >> --move to log
             listenAtEnd trans pid end conns
      ConnectionClosed cid -> listenAtEnd trans pid end $ Map.delete cid conns
      EndPointClosed -> putStrLn "Server Closing"
      _ -> putStrLn "Missing case") --move to log

-- |Handle the taken from a Received event, pid point to the node's message handler.
-- Waits for response and then passes the response back to the connection
handleData :: Transport -> MVar ProcessId -> [ByteString] -> Connection -> IO ()
handleData trans pid sentData conn = newLocalNode trans initRemoteTable >>=
  (\handleNode -> readMVar pid >>=
    (\pid' -> runProcess handleNode
      (getSelfPid >>=
      (\self -> Control.Distributed.Process.send pid' (self,TestMsg sentData)) >>
      receiveWait [match (sendResult conn)])))

-- |Takes the result of the servers work and sends it back to the client
sendResult :: Connection -> TestMsg -> Process ()
sendResult conn (TestMsg msg) =
  liftIO $ Network.Transport.send conn msg >>
  return ()

handleTest :: (ProcessId,TestMsg) -> Process ()
handleTest (pid,msg) = Control.Distributed.Process.send pid msg
