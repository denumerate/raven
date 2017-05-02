{-# LANGUAGE OverloadedStrings #-}
module Raven.Server.ServerNode
  ( newServerNode
  )where

import Network.Transport
import Control.Distributed.Process
import Control.Distributed.Process.Node
import Control.Concurrent
import Control.Monad (forever,void)

import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.ByteString.Char8 as B
import Data.Text (Text)

import Raven.Server.NodeMsgs
import Raven.Server.ConnNode
import Raven.Server.REPLNode
import Raven.Server.ResourceNode

-- |Maps connections to user ids
type ConnMap = Map ConnectionId Text

-- |Maps users to their info (Access)
type UserMap = Map Text (Bool,Maybe REPLNode)

-- |Builds a server node and all internal processes
newServerNode :: Transport -> EndPoint -> String -> IO ()
newServerNode trans end db = newLocalNode trans initRemoteTable >>=
  (\node -> putStrLn ("Server established at " ++ (show . address) end) >>
  newEmptyMVar >>=
    (\serverpid -> newMVar Map.empty >>=
      (\conMap -> newMVar Map.empty >>=
        (\uMap -> newResourceNode trans serverpid db >>=
          (\resNode -> runProcess node
            (liftIO (readMVar resNode) >>=
              (\resNode' ->
                 buildLogMsg ("Server established at " ++ (show . address) end) >>=
                Control.Distributed.Process.send resNode') >>
              spawnLocal (forever (receiveWait
                                   [ match (handleREPL trans conMap uMap serverpid)
                                   , match (handlePlot trans conMap uMap serverpid)
                                   , match (handleLog resNode)
                                   , match (handleLogin resNode)
                                   , match (handleLoginSuc conMap uMap)
                                   , match (handleLogout conMap)
                                   , match (handleKill conMap uMap
                                             trans end resNode)
                                   , match (handleREPLInfo conMap uMap)
                                   , match (handleStopREPL conMap uMap)
                                   , match (handleAllUsers conMap uMap resNode)
                                   , match (handleAddUser conMap uMap resNode)
                                   , match (handleDeleteUser conMap uMap resNode)
                                   , match (handleDeleteUserSucc uMap)
                                   , match (handleChangeRootAccess conMap uMap resNode)
                                   , match (handleRootAccessChanged uMap)
                                   , match (handleChangeUsersPassword conMap uMap resNode)
                                   , matchUnknown (catchAllMsgs' resNode "ServerNode")
                                   ])) >>=
              (\lpid -> liftIO (putMVar serverpid lpid) >>
                        (liftIO . listenAtEnd trans end node Map.empty) lpid) >>
              return ()))))))

-- |Handles a REPLMsg by looking for the users replNode (and handling errors)
-- and then sending the message.
-- If no node is found, one is created.
handleREPL :: Transport -> MVar ConnMap -> MVar UserMap -> MVar ProcessId ->
  (ConnectionId,REPLMsg) -> Process ()
handleREPL trans cMap uMap self msg@(cPID,REPLMsg n _) = void $ spawnLocal
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
       _ -> failed))
  where
    failed = Control.Distributed.Process.send cPID
             (ProcessedMsg n "Please Login")

-- |Handles a PlotMsg by looking for the users replNode (and handling errors)
-- and then sending the message.
-- If no node is found, one is created.
handlePlot :: Transport -> MVar ConnMap -> MVar UserMap -> MVar ProcessId ->
  (ConnectionId,PlotMsg) -> Process ()
handlePlot trans cMap uMap self msg@(cPID,PlotMsg n _ _) = void $ spawnLocal
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
       _ -> failed))
  where
    failed = Control.Distributed.Process.send cPID
             (ProcessedMsg n "Please Login")


-- |Handles a KillMsg by first ensuring that the user sending it has root access,
-- and then sending a kill message to all connected nodes.
-- Then closes the transport layer.
handleKill :: ProccessId -> MVar ConnMap -> MVar UserMap -> Transport -> EndPoint ->
  ResourceNode -> (cPId,KillMsg) -> Process ()
handleKill server cMap uMap trans end resNode (KillMsg n) =
  liftIO (readMVar cMap) >>=
  (\cMap' -> case Map.lookup cPID cMap' of
      Just id' -> liftIO (readMVar uMap) >>=
        (\uMap' -> case Map.lookup id' uMap' of
            Just (True,_) -> kill'
            Just (False,_) ->
              Control.Distributed.Process.send server
              (cPID,ProcessedMsg n "You cannot do that")
            _ -> Control.Distributed.Process.send server
              (cPID,ProcessedMsg n "Please Login"))
      _ -> Control.Distributed.Process.send server
           (cPID,ProcessedMsg n "Please Login"))
  where kill' =
          liftIO (putStrLn "Killing server (40 seconds)") >>
          liftIO (readMVar resNode) >>=
          (\resNode' ->
             buildLogMsg "Killing Server" >>=
             Control.Distributed.Process.send resNode' >>
             liftIO (threadDelay 5000000) >>
             Control.Distributed.Process.send resNode' (KillMsg "")) >>
          liftIO (readMVar uMap) >>=
          return . Map.map killUsers >>
          liftIO (closeEndPoint end) >>
          liftIO (threadDelay 35000000) >>
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
handleLogin :: ResourceNode -> (ConnectionId,LoginMsg) -> Process ()
handleLogin rNode msg = liftIO (readMVar rNode) >>=
  (`Control.Distributed.Process.send` msg)

-- |Handles a PlotDone message by passing it on to the resource node.
handlePlotDone :: ResourceNode -> (ConnectionId,PlotDoneMsg) -> Process ()
handlePlotDone rNode msg = liftIO (readMVar rNode) >>=
  (`Control.Distributed.Process.send` msg)

-- |Handles a successful login message by linking the connection to the user,
-- and then linking the user to its users information.
handleLoginSuc :: MVar ConnMap -> MVar UserMap -> (ConnectionId,LoginSucMsg) ->
  Process ()
handleLoginSuc cMap uMap (cPID,LoginSucMsg (id',rAcc)) = void $ spawnLocal
  (liftIO (takeMVar cMap) >>=
   (liftIO . putMVar cMap . Map.insert cPID id') >>
   liftIO (takeMVar uMap) >>=
   (liftIO . putMVar uMap . Map.insert id' (rAcc,Nothing)))

-- |Handles a logout message by removing the connection from the connection map,
-- does not modify any user information.
handleLogout :: MVar ConnMap -> (ConnectionId,LogoutMsg) -> Process ()
handleLogout cMap (cPID,LogoutMsg n) = void $ spawnLocal
  (liftIO (takeMVar cMap) >>=
   liftIO . putMVar cMap . Map.delete cPID >>
   Control.Distributed.Process.send cPID
    (ProcessedMsg n "Logged Out"))

-- |Handles a REPLInfoMsg by sending back if the user has an existing REPLNode
handleREPLInfo :: MVar ConnMap -> MVar UserMap -> (ConnectionId,REPLInfoMsg) ->
  Process ()
handleREPLInfo cMap uMap (cPID,REPLInfoMsg n) = void $ spawnLocal
  (liftIO (readMVar cMap) >>=
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
       _ -> failed))
  where
    failed = Control.Distributed.Process.send cPID
             (ProcessedMsg n "Please Login")

-- |Handles a StopREPLMSG by killing the connected repl node (if existing)
handleStopREPL :: MVar ConnMap -> MVar UserMap -> (ConnectionId,StopREPLMSG) ->
  Process ()
handleStopREPL cMap uMap (cPID,StopREPLMSG n) = void $ spawnLocal
  (liftIO (readMVar cMap) >>=
   return . Map.lookup cPID >>=
   (\cMap' -> case cMap' of
       Just uid ->
         liftIO (readMVar uMap) >>=
         return . Map.lookup uid >>=
         (\user -> case user of
             Just (acc,Just rNode) ->
               liftIO (readMVar rNode) >>=
               (`Control.Distributed.Process.send` (KillMsg "")) >>
               liftIO (takeMVar uMap) >>=
               liftIO . putMVar uMap . Map.insert uid (acc,Nothing) >>
               Control.Distributed.Process.send cPID
                   (ProcessedMsg n "REPL has been stopped")
             Just (_,Nothing) -> Control.Distributed.Process.send cPID
                                (ProcessedMsg n "REPL is not running")
             _ -> failed)
       _ -> failed))
  where
    failed = Control.Distributed.Process.send cPID
             (ProcessedMsg n "Please Login")

-- |Handles an AllUsersMsg by checking root access and, if allowed,
-- sending the request on to the resource node.
handleAllUsers :: MVar ConnMap -> MVar UserMap -> ResourceNode ->
  (ProcessId,AllUsersMsg) -> Process ()
handleAllUsers cMap uMap rNode msg@(cPID,AllUsersMsg n) = void $ spawnLocal
  (liftIO (readMVar cMap) >>=
   return . Map.lookup cPID >>=
   (\cMap' -> case cMap' of
       Just uid ->
         liftIO (readMVar uMap) >>=
         return . Map.lookup uid >>=
         (\user -> case user of
             Just (True,_) ->
               liftIO (readMVar rNode) >>=
               (`Control.Distributed.Process.send` msg)
             Just (False,_) -> Control.Distributed.Process.send cPID
                                (ProcessedMsg n "You do not have root access")
             _ -> failed)
       _ -> failed))
  where
    failed = Control.Distributed.Process.send cPID
             (ProcessedMsg n "Please Login")

-- |Handles an AddUserMsg by checking user permissions and then passing
-- the message on if allowed.
handleAddUser :: ProcessId -> MVar ConnMap -> MVar UserMap -> ResourceNode ->
  (ConnectionId,AddUserMsg) -> Process ()
handleAddUser server cMap uMap rNode msg@(cPID,AddUserMsg n _ _ _) =
  void $ spawnLocal
  (liftIO (readMVar cMap) >>=
   return . Map.lookup cPID >>=
   (\cMap' -> case cMap' of
       Just uid ->
         liftIO (readMVar uMap) >>=
         return . Map.lookup uid >>=
         (\user -> case user of
             Just (True,_) ->
               liftIO (readMVar rNode) >>=
               (`Control.Distributed.Process.send` msg)
             Just (False,_) -> Control.Distributed.Process.send server
                                (cPID,ProcessedMsg n "You do not have root access")
             _ -> failed)
       _ -> failed))
  where
    failed = Control.Distributed.Process.send server
             (cPID,ProcessedMsg n "Please Login")

-- |Handles an deleteUserMsg by checking user permissions and then passing
-- the message on if allowed.
handleDeleteUser :: ProcessId -> MVar ConnMap -> MVar UserMap -> ResourceNode ->
  (ConnectionId,DeleteUserMsg) -> Process ()
handleDeleteUser server cMap uMap rNode msg@(cPID,DeleteUserMsg n _) =
  void $ spawnLocal
  (liftIO (readMVar cMap) >>=
   return . Map.lookup cPID >>=
   (\cMap' -> case cMap' of
       Just uid ->
         liftIO (readMVar uMap) >>=
         return . Map.lookup uid >>=
         (\user -> case user of
             Just (True,_) ->
               liftIO (readMVar rNode) >>=
               (`Control.Distributed.Process.send` msg)
             Just (False,_) -> Control.Distributed.Process.send server
                                (cPID,ProcessedMsg n "You do not have root access")
             _ -> failed)
       _ -> failed))
  where
    failed = Control.Distributed.Process.send server
             (cPID,ProcessedMsg n "Please Login")

-- |Handles a DeleteUserSucc message by removing the id from the map and then
-- sending a message to the connNode.
handleDeleteUserSucc :: ProcessId -> MVar UserMap ->
  (ConnectionId,DeleteUserSuccMsg) -> Process ()
handleDeleteUserSucc server uMap (cPID,DeleteUserSuccMsg n id') = void $ spawnLocal
  (liftIO (takeMVar uMap) >>=
    (\uMap' -> return (Map.lookup id' uMap') >>=
      (\user -> case user of
          Just (_,Just rNode) -> liftIO (readMVar rNode) >>=
            (`Control.Distributed.Process.send` (KillMsg "")) >>
            Control.Distributed.Process.send server
                (cPID,ProcessedMsg n "User deleted") >>
            liftIO (putMVar uMap (Map.delete id' uMap'))
          Just _ ->
            Control.Distributed.Process.send server
                (cPID,ProcessedMsg n "User deleted") >>
            liftIO (putMVar uMap (Map.delete id' uMap'))
          _ ->
            Control.Distributed.Process.send server
                (cPID,ProcessedMsg n "User deleted") >>
            liftIO (putMVar uMap uMap'))))

-- |Handles an changeRootAccessMsg by checking user permissions and then passing
-- the message on if allowed.
handleChangeRootAccess :: ProcessId -> MVar ConnMap -> MVar UserMap -> ResourceNode ->
  (ConnectionId,ChangeRootAccessMsg) -> Process ()
handleChangeRootAccess server cMap uMap rNode msg@(cPID,ChangeRootAccessMsg n _ _) =
  void $ spawnLocal
  (liftIO (readMVar cMap) >>=
   return . Map.lookup cPID >>=
   (\cMap' -> case cMap' of
       Just uid ->
         liftIO (readMVar uMap) >>=
         return . Map.lookup uid >>=
         (\user -> case user of
             Just (True,_) ->
               liftIO (readMVar rNode) >>=
               (`Control.Distributed.Process.send` msg)
             Just (False,_) -> Control.Distributed.Process.send server
                                (cPID,ProcessedMsg n "You do not have root access")
             _ -> failed)
       _ -> failed))
  where
    failed = Control.Distributed.Process.send server
             (cPID,ProcessedMsg n "Please Login")

-- |Handles a RootAccessChangedMsg message by updating the usermap and then
-- sending a message to the connNode.
handleRootAccessChanged :: ProcessId -> MVar UserMap ->
  (ConnectionId,RootAccessChangedMsg) -> Process ()
handleRootAccessChanged server uMap (cPID,RootAccessChangedMsg n id' rAcc) =
  void $ spawnLocal (liftIO (takeMVar uMap) >>=
    (\uMap' -> return (Map.lookup id' uMap') >>=
      (\user -> case user of
          Just (_,node) ->
            Control.Distributed.Process.send server
                (cPID,ProcessedMsg n "User information updated") >>
            liftIO (putMVar uMap (Map.insert id' (rAcc,node) uMap'))
          _ ->
            Control.Distributed.Process.send server
                (cPID,ProcessedMsg n "User information updated") >>
            liftIO (putMVar uMap uMap'))))

-- |Handles a ChangeUsersPasswordMsg by checking user permissions and then passing
-- the message on if allowed.
handleChangeUsersPassword :: ProcessId -> MVar ConnMap -> MVar UserMap ->
  ResourceNode -> (ConnectionId,ChangeUsersPasswordMsg) -> Process ()
handleChangeUsersPassword server cMap uMap rNode msg@(cPID,ChangeUsersPasswordMsg n _ _) =
  void $ spawnLocal
  (liftIO (readMVar cMap) >>=
   return . Map.lookup cPID >>=
   (\cMap' -> case cMap' of
       Just uid ->
         liftIO (readMVar uMap) >>=
         return . Map.lookup uid >>=
         (\user -> case user of
             Just (True,_) ->
               liftIO (readMVar rNode) >>=
               (`Control.Distributed.Process.send` msg)
             Just (False,_) -> Control.Distributed.Process.send server
                                (cPID,ProcessedMsg n "You do not have root access")
             _ -> failed)
       _ -> failed))
  where
    failed = Control.Distributed.Process.send server
             (cPID,ProcessedMsg n "Please Login")
