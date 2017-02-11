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
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B

import Raven.REPL

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
                runProcess node
                (spawnLocal (liftIO (listenAtEnd end' Map.empty)) >>
                receiveWait []))
            _ -> putStrLn "Endpoint not initialized, Server Failed" >> --move to log
                 return ())
      _ -> putStrLn "Transport not initialized, Server Failed" >> --move to log
        return ())

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
              handleData info >>
              return ()
            Nothing -> putStrLn "Connection not found") >> --move to log
             listenAtEnd end conns
      ConnectionClosed cid -> listenAtEnd end $ Map.delete cid conns
      EndPointClosed -> putStrLn "Server Closing"
      _ -> putStrLn "Missing case") --move to log

handleData :: [ByteString] -> Connection -> IO ()
handleData sentData conn =
  Network.Transport.send conn sentData >>
  return ()
