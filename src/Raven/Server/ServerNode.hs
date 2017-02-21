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

connTimeout = 1800000000

-- |Builds a server node and all internal processes
newServerNode :: Transport -> EndPoint -> IO ()
newServerNode trans end = newLocalNode trans initRemoteTable >>=
  (\node -> putStrLn ("Server established at " ++ (show . address) end) >>
  newEmptyMVar >>=
    (\serverpid -> newREPLNode trans serverpid >>=
      (\replNode -> newResourceNode trans >>=
        (\resNode -> runProcess node
          (liftIO (takeMVar resNode) >>=
           (\resNode' ->
              buildLogMsg ("Server established at " ++ (show . address) end) >>=
              Control.Distributed.Process.send resNode') >>
            spawnLocal (forever (receiveWait
                                [ match (handleREPL replNode)
                                , match (handleLog resNode)
                                , match (handleKill trans end replNode resNode)
                                , matchUnknown (catchAllMsgs' resNode "ServerNode")
                                ])) >>=
           (\lpid -> (spawnLocal . liftIO . listenAtEnd trans end node Map.empty) lpid >>
                     liftIO (putMVar serverpid lpid)) >>
           return ())))))

-- |Listen for and handle events from the endpoint.
-- The pid is the id of the node's listening process
listenAtEnd :: Transport -> EndPoint -> LocalNode ->
  Map ConnectionId (MVar ConnNode) -> ProcessId -> IO ()
listenAtEnd trans end serverN conns pid = receive end >>=
  (\event -> case event of
      ConnectionOpened cid reliabilty adrs -> newEmptyMVar >>=
             (\cNode -> forkIO
               (Network.Transport.connect end adrs reliabilty
                (defaultConnectHints{connectTimeout = Just connTimeout}) >>=
                 (\conn -> case conn of
                     Right conn' -> newConnNode trans pid conn' >>=
                           putMVar cNode
                     Left err ->
                       runProcess serverN
                           (buildLogMsg
                             ("Connection not established with " ++ show adrs
                               ++ ": " ++ show err) >>=
                             Control.Distributed.Process.send pid))) >>
               listenAtEnd trans end serverN (Map.insert cid cNode conns) pid)
      Received cid info -> forkIO
             (case Map.lookup cid conns of
                 Just conn -> readMVar conn >>=
                              handleReceived pid info
                 Nothing -> runProcess serverN
                            (buildLogMsg "Connection not found in Map" >>=
                             Control.Distributed.Process.send pid)) >>
             listenAtEnd trans end serverN conns pid
      ConnectionClosed cid -> readMVar (conns Map.! cid) >>=
                              cleanConnNode >>
                              listenAtEnd trans end serverN
                              (Map.delete cid conns) pid
      EndPointClosed -> putStrLn "EndPoint Closed" >>
                        return (Map.map clean conns) >>
                        return ()
      _ -> forkIO (runProcess serverN
                   (buildLogMsg "Uncaught event at endpoint" >>=
                    Control.Distributed.Process.send pid)) >>
           listenAtEnd trans end serverN conns pid)
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
      buildLogMsg "Killing Server" >>=
      Control.Distributed.Process.send resNode' >>
      liftIO (threadDelay 5000000) >>
      Control.Distributed.Process.send resNode' KillMsg) >>
  liftIO (readMVar replNode) >>=
  (`Control.Distributed.Process.send` KillMsg) >>
  liftIO (closeEndPoint end) >>
  liftIO (threadDelay 25000000) >>
  liftIO (closeTransport trans)

-- |Handle a LogMsg
handleLog :: ResourceNode -> LogMsg -> Process ()
handleLog rNode msg = liftIO (readMVar rNode) >>=
  (`Control.Distributed.Process.send` msg)
