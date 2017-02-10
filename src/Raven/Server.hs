module Raven.Server
  ( initServer
  ) where

import Network
import Network.Socket
import Network.Transport
import Network.Transport.TCP
import Control.Distributed.Process
import Control.Distributed.Process.Node
import Control.Concurrent
import Control.Monad (forever)

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
                (\conns-> runProcess node $
                  spawnLocal $ forever $ liftIO $ listenAtEnd end' conns >>
                  return ()))
            _ -> putStrLn "Endpoint not initialized, Server Failed") --move to log
      _ -> putStrLn "Transport not initialized, Server Failed") --move to log

-- |Listen for and handle events from the endpoint
listenAtEnd :: EndPoint -> MVar (Map ConnectionId Connection) -> IO ()
listenAtEnd end conns = receive end >>=
  (\event -> case event of
      ConnectionOpened cid reliabilty address ->
        Network.Transport.connect end address reliabilty defaultConnectHints >>=
        (\conn -> case conn of
            Right conn' -> takeMVar conns >>=
              (\conns' -> putMVar conns $ Map.insert cid conn' conns')
            _ -> putStrLn "Connection failed") --move to log
  )
