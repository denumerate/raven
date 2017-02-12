module Raven.Server
  ( initServer
  ) where

import Network
import Network.Transport
import Network.Transport.TCP
import Control.Distributed.Process
import Control.Distributed.Process.Node
import Control.Concurrent
import Control.Monad (forever)

import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.ByteString.Char8 as B

import Raven.Server.NodeMsgs
import Raven.Server.ConnNode
import Raven.Server.REPLNode
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
                newREPLNode trans' >>=
                (\replNode -> newEmptyMVar >>=
                  (\pid ->
                    runProcess node
                    (spawnLocal (liftIO (listenAtEnd trans' pid end' Map.empty)) >>
                     spawnLocal (forever (receiveWait
                                          [match (handleREPL replNode)])) >>=
                     liftIO . putMVar pid))))
            _ -> putStrLn "Endpoint not initialized, Server Failed" >> --move to log
                 return ())
      _ -> putStrLn "Transport not initialized, Server Failed" >> --move to log
        return ())

-- |Listen for and handle events from the endpoint.
-- The pid is the id of the node's listening process
listenAtEnd :: Transport -> MVar ProcessId -> EndPoint ->
  Map ConnectionId (MVar ConnNode) -> IO ()
listenAtEnd trans pid end conns = receive end >>=
  (\event -> case event of
      ConnectionOpened cid reliabilty adrs -> newEmptyMVar >>=
        (\cNode -> forkIO
          (Network.Transport.connect end adrs reliabilty defaultConnectHints >>=
           (\conn -> case conn of
               Right conn' -> newConnNode trans conn' >>=
                 putMVar cNode
               _ -> putStrLn "Connection failed")) >> --move to log
          listenAtEnd trans pid end (Map.insert cid cNode conns))
      Received cid info -> forkIO
        (case Map.lookup cid conns of
            Just conn -> readMVar conn >>=
              handleReceived pid info >>
              return ()
            Nothing -> putStrLn "Connection not found") >> --move to log
             listenAtEnd trans pid end conns
      ConnectionClosed cid -> listenAtEnd trans pid end $ Map.delete cid conns
      EndPointClosed -> putStrLn "Server Closing"
      _ -> putStrLn "Missing case") --move to log

handleREPL :: REPLNode -> (ProcessId,REPLMsg) -> Process ()
handleREPL replNode msg = liftIO (readMVar replNode) >>=
  (\replNode' -> Control.Distributed.Process.send replNode' msg)
