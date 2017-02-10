module Raven.Server
  ( initServer
  ) where

import Network
import Network.Transport
import Network.Transport.TCP
import Control.Distributed.Process
import Control.Distributed.Process.Node
import Control.Concurrent

import Data.Map (Map)
import qualified Data.Map as Map

import Raven.REPL

-- |Start the server and listen for connections at the supplied port number.
-- Creates a new thread to handle the connection
initServer :: Int -> IO ()
initServer portNum = withSocketsDo $
  createTransport "127.0.0.1" (show portNum) defaultTCPParameters >>=
  (\trans -> case trans of
      Right trans' -> newEndPoint trans' >>=
        (\end -> case end of
            Right end' -> newLocalNode trans' initRemoteTable >>=
              (\node -> newMVar Map.empty >>=
                runProcess node . liftIO . listenAtEnd end' >>
                return ())
            _ -> putStrLn "Endpoint not initialized, Server Failed") --move to log
      _ -> putStrLn "Transport not initialized, Server Failed") --move to log

-- |Listen for and handle events from the endpoint
listenAtEnd :: EndPoint -> MVar (Map ConnectionId Connection) -> IO ()
listenAtEnd end conns = receive end >>=
  (\event -> case event of
      ConnectionOpened cid reliabilty adrs -> forkIO
        (Network.Transport.connect end adrs reliabilty defaultConnectHints >>=
        (\conn -> case conn of
            Right conn' -> takeMVar conns >>=
              (\conns' -> putMVar conns $ Map.insert cid conn' conns') >>
              return ()
            _ -> putStrLn "Connection failed")) >> --move to log
             listenAtEnd end conns
      Received cid info -> forkIO
        (readMVar conns >>=
        (\conns' -> case Map.lookup cid conns' of
            Just conn -> Network.Transport.send conn info >>
              return ()
            Nothing -> putStrLn "Connection not found")) >> --move to log
             listenAtEnd end conns
      ConnectionClosed cid -> forkIO
        (takeMVar conns >>=
         (\conns' -> putMVar conns (Map.delete cid conns'))) >>
        listenAtEnd end conns
      _ -> putStrLn "Missing case")
