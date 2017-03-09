{-# LANGUAGE OverloadedStrings #-}
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
import qualified Data.ByteString.Char8 as B
import Data.Text (Text)

import Raven.Server.NodeMsgs
import Raven.Server.ConnNode
import Raven.Server.REPLNode
import Raven.Server.ResourceNode

-- |Maps connections to user ids
type ConnMap = Map ProcessId Text

-- |Maps users to their info (Access)
type UserMap = Map Text (Bool,Maybe REPLNode)

connTimeout = 1800000000
tokenSize = 13

-- |Builds a server node and all internal processes
newServerNode :: Transport -> EndPoint -> IO ()
newServerNode trans end = newLocalNode trans initRemoteTable >>=
  (\node -> putStrLn ("Server established at " ++ (show . address) end) >>
  newEmptyMVar >>=
    (\serverpid -> newMVar Map.empty >>=
      (\conMap -> newMVar Map.empty >>=
        (\uMap -> newResourceNode trans serverpid >>=
          (\resNode -> runProcess node
            (liftIO (readMVar resNode) >>=
              (\resNode' ->
                 buildLogMsg ("Server established at " ++ (show . address) end) >>=
                Control.Distributed.Process.send resNode') >>
              spawnLocal (forever (receiveWait
                                   [ match (handleREPL trans conMap uMap serverpid)
                                   , match (handleLog resNode)
                                   , match (handleLogin resNode)
                                   , match (handleLoginSuc conMap uMap)
                                   , match (handleLogout conMap)
                                   , match (handleKill conMap uMap
                                             trans end resNode)
                                   , match (handleREPLInfo conMap uMap)
                                   , matchUnknown (catchAllMsgs' resNode "ServerNode")
                                   ])) >>=
              (\lpid -> liftIO (putMVar serverpid lpid) >>
                        (liftIO . listenAtEnd trans end node Map.empty) lpid) >>
              return ()))))))

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
                             ("Connection failed with " ++ show adrs
                               ++ ". " ++ show err) >>=
                             Control.Distributed.Process.send pid))) >>
               listenAtEnd trans end serverN (Map.insert cid cNode conns) pid)
      Received cid [info] -> forkIO
             (case Map.lookup cid conns of
                 Just conn -> let wds = B.words info in
                                readMVar conn >>=
                                handleReceived pid wds
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
      ErrorEvent (TransportError _ err) ->
        (runProcess serverN
          (buildLogMsg ("EndPoint Error: " ++ err) >>=
            Control.Distributed.Process.send pid)) >>
             listenAtEnd trans end serverN conns pid
      _ -> forkIO (runProcess serverN
                   (buildLogMsg "Uncaught event at endpoint" >>=
                    Control.Distributed.Process.send pid)) >>
           listenAtEnd trans end serverN conns pid)
  where
    clean val =
      readMVar val >>=
      cleanConnNode

-- |Handles a REPLMsg by looking for the users replNode (and handling errors)
-- and then sending the message.
-- If no node is found, one is created.
handleREPL :: Transport -> MVar ConnMap -> MVar UserMap -> MVar ProcessId ->
  (ProcessId,REPLMsg) -> Process ()
handleREPL trans cMap uMap self msg@(cPID,REPLMsg n _) = spawnLocal
  (liftIO (readMVar cMap) >>=
   return . Map.lookup cPID >>=
   (\uid -> case uid of
       Just uid' -> liftIO (readMVar uMap) >>=
            return . Map.lookup uid' >>=
            (\usr -> case usr of
                Just (_,Just rNode) ->
                  liftIO (readMVar rNode) >>=
                  (`Control.Distributed.Process.send` msg)
                Just (acc,Nothing) ->
                  liftIO (takeMVar uMap) >>=
                  (\tUMap ->
                     liftIO (newREPLNode trans self) >>=
                     (\rNode ->
                        liftIO (readMVar rNode) >>=
                        (`Control.Distributed.Process.send` msg) >>
                        liftIO (putMVar uMap
                                (Map.insert uid' (acc,Just rNode) tUMap))))
                _ -> failed)
       _ -> failed)) >>
  return ()
  where
    failed = Control.Distributed.Process.send cPID
             (ProcessedMsg n "Please Login")

-- |Handles a KillMsg by first ensuring that the user sending it has root access,
-- and then sending a kill message to all connected nodes.
-- Then closes the transport layer.
handleKill :: MVar ConnMap -> MVar UserMap -> Transport -> EndPoint ->
  ResourceNode -> (ProcessId,KillMsg) -> Process ()
handleKill cMap uMap trans end resNode (cPID,(KillMsg n)) =
  liftIO (readMVar cMap) >>=
  (\cMap' -> case Map.lookup cPID cMap' of
      Just id' -> liftIO (readMVar uMap) >>=
        (\uMap' -> case Map.lookup id' uMap' of
            Just (True,_) -> kill'
            Just (False,_) ->
              Control.Distributed.Process.send cPID
              (ProcessedMsg n "You cannot do that")
            _ -> Control.Distributed.Process.send cPID
              (ProcessedMsg n "Please Login"))
      _ -> Control.Distributed.Process.send cPID
           (ProcessedMsg n "Please Login"))
  where kill' =
          liftIO (putStrLn "Killing server (30 seconds)") >>
          liftIO (readMVar resNode) >>=
          (\resNode' ->
             buildLogMsg "Killing Server" >>=
            Control.Distributed.Process.send resNode' >>
            liftIO (threadDelay 5000000) >>
            Control.Distributed.Process.send resNode' (KillMsg "")) >>
          liftIO (readMVar uMap) >>=
          return . Map.map killUsers >>
          liftIO (closeEndPoint end) >>
          liftIO (threadDelay 25000000) >>
          liftIO (closeTransport trans)
        killUsers (_,Nothing) = return ()
        killUsers (_,Just rNode) =
          liftIO (readMVar rNode) >>=
          (`Control.Distributed.Process.send` (KillMsg "")) >>
          return ()

-- |Handles a LogMsg by sending it on to the resource node.
handleLog :: ResourceNode -> LogMsg -> Process ()
handleLog rNode msg = liftIO (readMVar rNode) >>=
  (`Control.Distributed.Process.send` msg)

-- |Handles a Login message by passing it on to the resource node.
handleLogin :: ResourceNode -> (ProcessId,LoginMsg) -> Process ()
handleLogin rNode msg = liftIO (readMVar rNode) >>=
  (`Control.Distributed.Process.send` msg)

-- |Handles a successful login message by linking the connection to the user,
-- and then linking the user to its users information.
handleLoginSuc :: MVar ConnMap -> MVar UserMap -> (ProcessId,LoginSucMsg) ->
  Process ()
handleLoginSuc cMap uMap (cPID,LoginSucMsg (id',rAcc)) = spawnLocal
  (liftIO (takeMVar cMap) >>=
   (liftIO . putMVar cMap . Map.insert cPID id') >>
   liftIO (takeMVar uMap) >>=
   (liftIO . putMVar uMap . Map.insert id' (rAcc,Nothing))) >>
  return ()

-- |Handles a logout message by removing the connection from the connection map,
-- does not modify any user information.
handleLogout :: MVar ConnMap -> (ProcessId,LogoutMsg) -> Process ()
handleLogout cMap (cPID,LogoutMsg n) = spawnLocal
  (liftIO (takeMVar cMap) >>=
   liftIO . putMVar cMap . Map.delete cPID >>
   Control.Distributed.Process.send cPID
    (ProcessedMsg n "Logged Out")) >>
  return ()

-- |Handles a REPLInfoMsg by sending back if the user has an existing REPLNode
handleREPLInfo :: MVar ConnMap -> MVar UserMap -> (ProcessId,REPLInfoMsg) ->
  Process ()
handleREPLInfo cMap uMap (cPID,REPLInfoMsg n) =
  liftIO (readMVar cMap) >>=
  return . Map.lookup cPID >>=
  (\cMap' -> case cMap' of
      Just uid ->
        liftIO (readMVar uMap) >>=
        return . Map.lookup uid >>=
        (\user -> case user of
            Just (_,Just _) -> Control.Distributed.Process.send cPID
                               (ProcessedMsg n "REPL is running")
            Just (_,Nothing) -> Control.Distributed.Process.send cPID
                                (ProcessedMsg n "REPL is not running")
            _ -> failed)
      _ -> failed)
  where
    failed = Control.Distributed.Process.send cPID
             (ProcessedMsg n "Please Login")
