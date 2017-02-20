module Raven.Server.ServerNode
  ( newServerNode
  )where

import Network.Transport
import Control.Distributed.Process
import Control.Distributed.Process.Node
import Control.Concurrent
import Control.Monad (forever)

import Data.Map (Map)
import qualified Data.Map as Map

import Raven.Server.NodeMsgs
import Raven.Server.ConnNode
import Raven.Server.REPLNode
import Raven.Server.ResourceNode

-- |Builds a server node and all internal processes
newServerNode :: Transport -> EndPoint -> IO ()
newServerNode trans end = newLocalNode trans initRemoteTable >>=
  (\node -> putStrLn ("Server established at " ++ (show . address) end) >>
  newEmptyMVar >>=
    (\serverpid -> newREPLNode trans serverpid >>=
      (\replNode -> newResourceNode trans >>=
        (\resNode -> runProcess node
          (liftIO (takeMVar resNode) >>=
           (`Control.Distributed.Process.send`
             (buildLogMsg ("Server established at " ++ (show . address) end)))
            spawnLocal (forever (receiveWait
                                [ match (handleREPL replNode)
                                , match (handleKill trans end replNode)
                                , matchUnknown (catchAllMsgs' resNode "ServerNode")
                                ])) >>=
           (\lpid -> (spawnLocal . liftIO . listenAtEnd trans end Map.empty) lpid >>
                     liftIO (putMVar serverpid lpid)) >>
           return ())))))

-- |Listen for and handle events from the endpoint.
-- The pid is the id of the node's listening process
listenAtEnd :: Transport -> EndPoint ->
  Map ConnectionId (MVar ConnNode) -> ProcessId -> IO ()
listenAtEnd trans end conns pid = receive end >>=
  (\event -> case event of
      ConnectionOpened cid reliabilty adrs -> newEmptyMVar >>=
             (\cNode -> forkIO
               (Network.Transport.connect end adrs reliabilty defaultConnectHints >>=
                 (\conn -> case conn of
                     Right conn' -> newConnNode trans pid conn' >>=
                           putMVar cNode
                     _ -> putStrLn "Connection failed")) >> --move to log
                 listenAtEnd trans end (Map.insert cid cNode conns) pid)
      Received cid info -> forkIO
             (case Map.lookup cid conns of
                 Just conn -> readMVar conn >>=
                              handleReceived pid info >>
                              return ()
                 Nothing -> putStrLn "Connection not found") >> --move to log
             listenAtEnd trans end conns pid
      ConnectionClosed cid -> readMVar (conns Map.! cid) >>=
                              cleanConnNode >>
                              listenAtEnd trans end
                              (Map.delete cid conns) pid
      EndPointClosed -> putStrLn "Server Closing" >> --log?
                        return (Map.map clean conns) >>
                        return ()
      _ -> putStrLn "Missing case") --move to log
  where
    clean val =
      readMVar val >>=
      cleanConnNode

-- |Handles a REPLMsg
handleREPL :: REPLNode -> (ProcessId,REPLMsg) -> Process ()
handleREPL replNode msg = liftIO (readMVar replNode) >>=
  (\replNode' -> Control.Distributed.Process.send replNode' msg)

-- |Handles a KillMsg
handleKill :: Transport -> EndPoint -> REPLNode -> ResourceNode -> KillMsg -> Process ()
handleKill trans end replNode resNode _ = liftIO (readMVar resNode) >>=
  (\resNode' ->
     Control.Distributed.Process.send resNode' (buildLogMsg "Killing Server") >>
     liftIO (threadDelay 5000000)
     Control.Distributed.Process.send resNode' KillMsg) >>
  liftIO (readMVar replNode) >>=
  (`Control.Distributed.Process.send` KillMsg) >>
  liftIO (closeEndPoint end) >>
  liftIO (threadDelay 25000000) >>
  liftIO (closeTransport trans)
