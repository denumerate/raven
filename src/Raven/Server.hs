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

-- |Start the server and listen for connections at the supplied ip:port number.
-- Returns the address of the server endpoint (if successful)
initServer :: String -> String -> IO (Maybe EndPointAddress)
initServer ip portNum = withSocketsDo $
  createTransport ip portNum defaultTCPParameters >>=
  (\trans -> case trans of
      Right trans' -> newEndPoint trans' >>=
        (\end -> case end of
            Right end' -> newLocalNode trans' initRemoteTable >>=
              (\node -> (runProcess node . liftIO . listenAtEnd end') Map.empty >>
                return (Just (address end')))
            _ -> putStrLn "Endpoint not initialized, Server Failed" >> --move to log
                 return Nothing)
      _ -> putStrLn "Transport not initialized, Server Failed" >> --move to log
        return Nothing)

-- |Listen for and handle events from the endpoint
listenAtEnd :: EndPoint -> Map ConnectionId (MVar Connection) -> IO ()
listenAtEnd end conns = receive end >>=
  (\event -> case event of
      ConnectionOpened cid reliabilty adrs -> newEmptyMVar >>=
        (\cEntry -> forkIO
          (Network.Transport.connect end adrs reliabilty defaultConnectHints >>=
           (\conn -> case conn of
               Right conn' -> putMVar cEntry conn' >>
                 return ()
               _ -> putStrLn "Connection failed")) >> --move to log
          listenAtEnd end (Map.insert cid cEntry conns))
      Received cid info -> forkIO
        (case Map.lookup cid conns of
            Just conn -> readMVar conn >>=
              (\conn' -> Network.Transport.send conn' info) >>
              return ()
            Nothing -> putStrLn "Connection not found") >> --move to log
             listenAtEnd end conns
      ConnectionClosed cid -> listenAtEnd end $ Map.delete cid conns
      EndPointClosed -> putStrLn "Server Closing"
      _ -> putStrLn "Missing case")
