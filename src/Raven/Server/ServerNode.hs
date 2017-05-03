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
import Data.Text (Text)

import Raven.Server.NodeMsgs
import Raven.Server.Connection
import Raven.Server.REPLNode
import Raven.Server.ResourceNode

-- |Maps connections to user ids
type ConnMap = Map ConnectionId Text

-- |Maps users to their info (Access)
type UserMap = Map Text Bool

-- |Builds a server node and all internal processes
newServerNode :: Transport -> EndPoint -> String -> IO ()
newServerNode trans end db = newLocalNode trans initRemoteTable >>=
  (\node -> putStrLn ("Server established at " ++ (show . address) end) >>
  newEmptyMVar >>=
    (\serverpid -> newMVar Map.empty >>=
      (\conMap -> newMVar Map.empty >>=
        (\uMap -> newMVar Map.empty >>=
          (\sendMap -> newResourceNode trans serverpid db >>=
            (\resNode -> newREPLNode trans serverpid >>=
              (\replNode -> runProcess node
                (liftIO (readMVar resNode) >>=
                 (\resNode' ->
                    buildLogMsg ("Server established at " ++ (show . address) end) >>=
                    Control.Distributed.Process.send resNode') >>
                 spawnLocal (forever (receiveWait
                                      [ match (sendResult sendMap serverpid)
                                      , match (handleREPL replNode)
                                      , match (handlePlot replNode)
                                      , match (handleLog resNode)
                                      , match (handleLogin resNode)
                                      , match (handleLoginSuc conMap uMap)
                                      , match (handleLogout serverpid conMap)
                                      , match (handleKill serverpid conMap uMap
                                               trans end resNode replNode)
                                      , match (handleAllUsers serverpid conMap uMap resNode)
                                      , match (handleAddUser serverpid conMap uMap resNode)
                                      , match (handleDeleteUser serverpid conMap uMap resNode)
                                      , match (handleDeleteUserSucc serverpid uMap)
                                      , match (handleChangeRootAccess serverpid conMap uMap
                                               resNode)
                                      , match (handleRootAccessChanged serverpid uMap)
                                      , match (handleChangeUsersPassword serverpid conMap
                                               uMap resNode)
                                      , matchUnknown (catchAllMsgs' resNode "ServerNode")
                                      ])) >>=
                 (\lpid -> liftIO (putMVar serverpid lpid) >>
                           (liftIO . listenAtEnd trans end node sendMap) lpid) >>
                 return ()))))))))

-- |Handles a REPLMsg by looking for the users replNode (and handling errors)
-- and then sending the message.
-- If no node is found, one is created.
handleREPL :: REPLNode -> (ConnectionId,PlotMsg) -> Process ()
handleREPL rNode msg = void $ spawnLocal $
  liftIO (readMVar rNode) >>=
  (`Control.Distributed.Process.send` msg)

-- |Handles a PlotMsg by looking for the users replNode (and handling errors)
-- and then sending the message.
-- If no node is found, one is created.
handlePlot :: REPLNode -> (ConnectionId,PlotMsg) -> Process ()
handlePlot rNode msg = void $ spawnLocal $
  liftIO (readMVar rNode) >>=
  (`Control.Distributed.Process.send` msg)

-- |Handles a KillMsg by first ensuring that the user sending it has root access,
-- and then sending a kill message to all connected nodes.
-- Then closes the transport layer.
handleKill :: MVar ProcessId -> MVar ConnMap -> MVar UserMap -> Transport -> EndPoint ->
  ResourceNode -> REPLNode -> (ConnectionId,KillMsg) -> Process ()
handleKill server cMap uMap trans end resNode replNode (cPID,KillMsg n) =
  liftIO (readMVar server) >>=
  (\server' ->
     liftIO (readMVar cMap) >>=
     (\cMap' -> case Map.lookup cPID cMap' of
         Just id' -> liftIO (readMVar uMap) >>=
           (\uMap' -> case Map.lookup id' uMap' of
               Just True -> kill'
               Just False ->
                 Control.Distributed.Process.send server'
                 (cPID,ProcessedMsg n "You cannot do that")
               _ -> Control.Distributed.Process.send server'
                 (cPID,ProcessedMsg n "Please Login"))
         _ -> Control.Distributed.Process.send server'
              (cPID,ProcessedMsg n "Please Login")))
  where kill' =
          liftIO (putStrLn "Killing server (40 seconds)") >>
          liftIO (readMVar resNode) >>=
          (\resNode' ->
             buildLogMsg "Killing Server" >>=
             Control.Distributed.Process.send resNode' >>
             liftIO (threadDelay 5000000) >>
             Control.Distributed.Process.send resNode' (KillMsg "")) >>
          liftIO (readMVar replNode) >>=
          (`Control.Distributed.Process.send` (KillMsg "")) >>
          liftIO (closeEndPoint end) >>
          liftIO (threadDelay 35000000) >>
          liftIO (closeTransport trans)

-- |Handles a LogMsg by sending it on to the resource node.
handleLog :: ResourceNode -> LogMsg -> Process ()
handleLog rNode msg = liftIO (readMVar rNode) >>=
  (`Control.Distributed.Process.send` msg)

-- |Handles a Login message by passing it on to the resource node.
handleLogin :: ResourceNode -> (ConnectionId,LoginMsg) -> Process ()
handleLogin rNode msg = liftIO (readMVar rNode) >>=
  (`Control.Distributed.Process.send` msg)

-- |Handles a successful login message by linking the connection to the user,
-- and then linking the user to its users information.
handleLoginSuc :: MVar ConnMap -> MVar UserMap -> (ConnectionId,LoginSucMsg) ->
  Process ()
handleLoginSuc cMap uMap (cPID,LoginSucMsg (id',rAcc)) = void $ spawnLocal
  (liftIO (takeMVar cMap) >>=
   (liftIO . putMVar cMap . Map.insert cPID id') >>
   liftIO (takeMVar uMap) >>=
   (liftIO . putMVar uMap . Map.insert id' rAcc))

-- |Handles a logout message by removing the connection from the connection map,
-- does not modify any user information.
handleLogout :: MVar ProcessId -> MVar ConnMap -> (ConnectionId,LogoutMsg) -> Process ()
handleLogout server cMap (cPID,LogoutMsg n) = void $ spawnLocal
  (liftIO (takeMVar cMap) >>=
   liftIO . putMVar cMap . Map.delete cPID >>
   liftIO (takeMVar server) >>=
   (`Control.Distributed.Process.send`
     (cPID,ProcessedMsg n "Logged Out")))

-- |Handles an AllUsersMsg by checking root access and, if allowed,
-- sending the request on to the resource node.
handleAllUsers :: MVar ProcessId -> MVar ConnMap -> MVar UserMap -> ResourceNode ->
  (ConnectionId,AllUsersMsg) -> Process ()
handleAllUsers server cMap uMap rNode msg@(cPID,AllUsersMsg n) = void $ spawnLocal
  (liftIO (readMVar server) >>=
   (\server' ->
      fmap (Map.lookup cPID) (liftIO (readMVar cMap)) >>=
      (\cMap' -> case cMap' of
          Just uid ->
            fmap (Map.lookup uid) (liftIO (readMVar uMap)) >>=
            (\user -> case user of
                Just True ->
                  liftIO (readMVar rNode) >>=
                  (`Control.Distributed.Process.send` msg)
                Just False -> Control.Distributed.Process.send server'
                                (cPID,ProcessedMsg n "You do not have root access")
                _ -> failed server')
          _ -> failed server')))
  where
    failed server' = Control.Distributed.Process.send server'
             (cPID,ProcessedMsg n "Please Login")

-- |Handles an AddUserMsg by checking user permissions and then passing
-- the message on if allowed.
handleAddUser :: MVar ProcessId -> MVar ConnMap -> MVar UserMap -> ResourceNode ->
  (ConnectionId,AddUserMsg) -> Process ()
handleAddUser server cMap uMap rNode msg@(cPID,AddUserMsg n _ _ _) =
  void $ spawnLocal
  (liftIO (readMVar server) >>=
   (\server' ->
      fmap (Map.lookup cPID) (liftIO (readMVar cMap)) >>=
      (\cMap' -> case cMap' of
          Just uid ->
            fmap (Map.lookup uid) (liftIO (readMVar uMap)) >>=
            (\user -> case user of
                Just True ->
                  liftIO (readMVar rNode) >>=
                  (`Control.Distributed.Process.send` msg)
                Just False -> Control.Distributed.Process.send server'
                                  (cPID,ProcessedMsg n "You do not have root access")
                _ -> failed server')
          _ -> failed server')))
  where
    failed server' = Control.Distributed.Process.send server'
             (cPID,ProcessedMsg n "Please Login")

-- |Handles an deleteUserMsg by checking user permissions and then passing
-- the message on if allowed.
handleDeleteUser :: MVar ProcessId -> MVar ConnMap -> MVar UserMap -> ResourceNode ->
  (ConnectionId,DeleteUserMsg) -> Process ()
handleDeleteUser server cMap uMap rNode msg@(cPID,DeleteUserMsg n _) =
  void $ spawnLocal
  (liftIO (readMVar server) >>=
   (\server' ->
      fmap (Map.lookup cPID) (liftIO (readMVar cMap)) >>=
      (\cMap' -> case cMap' of
          Just uid ->
            fmap (Map.lookup uid) (liftIO (readMVar uMap)) >>=
            (\user -> case user of
                Just True ->
                  liftIO (readMVar rNode) >>=
                  (`Control.Distributed.Process.send` msg)
                Just False -> Control.Distributed.Process.send server'
                                  (cPID,ProcessedMsg n "You do not have root access")
                _ -> failed server')
          _ -> failed server')))
  where
    failed server' = Control.Distributed.Process.send server'
             (cPID,ProcessedMsg n "Please Login")

-- |Handles a DeleteUserSucc message by removing the id from the map and then
-- sending a message to the connNode.
handleDeleteUserSucc :: MVar ProcessId -> MVar UserMap ->
  (ConnectionId,DeleteUserSuccMsg) -> Process ()
handleDeleteUserSucc server uMap (cPID,DeleteUserSuccMsg n id') = void $ spawnLocal
  (liftIO (readMVar server) >>=
   (\server' ->
      liftIO (takeMVar uMap) >>=
      (\uMap' -> return (Map.lookup id' uMap') >>=
        (\user -> case user of
            Just _ ->
              Control.Distributed.Process.send server'
                  (cPID,ProcessedMsg n "User deleted") >>
              liftIO (putMVar uMap (Map.delete id' uMap'))
            _ ->
              Control.Distributed.Process.send server'
                  (cPID,ProcessedMsg n "User deleted") >>
              liftIO (putMVar uMap uMap')))))

-- |Handles an changeRootAccessMsg by checking user permissions and then passing
-- the message on if allowed.
handleChangeRootAccess :: MVar ProcessId -> MVar ConnMap -> MVar UserMap -> ResourceNode ->
  (ConnectionId,ChangeRootAccessMsg) -> Process ()
handleChangeRootAccess server cMap uMap rNode msg@(cPID,ChangeRootAccessMsg n _ _) =
  void $ spawnLocal
  (liftIO (readMVar server) >>=
   (\server' ->
      fmap (Map.lookup cPID) (liftIO (readMVar cMap)) >>=
      (\cMap' -> case cMap' of
          Just uid ->
            fmap (Map.lookup uid) (liftIO (readMVar uMap)) >>=
            (\user -> case user of
                Just True ->
                  liftIO (readMVar rNode) >>=
                  (`Control.Distributed.Process.send` msg)
                Just False -> Control.Distributed.Process.send server'
                                  (cPID,ProcessedMsg n "You do not have root access")
                _ -> failed server')
          _ -> failed server')))
  where
    failed server' = Control.Distributed.Process.send server'
             (cPID,ProcessedMsg n "Please Login")

-- |Handles a RootAccessChangedMsg message by updating the usermap and then
-- sending a message to the connNode.
handleRootAccessChanged :: MVar ProcessId -> MVar UserMap ->
  (ConnectionId,RootAccessChangedMsg) -> Process ()
handleRootAccessChanged server uMap (cPID,RootAccessChangedMsg n id' rAcc) =
  void $ spawnLocal
  (liftIO (readMVar server) >>=
   (\server' ->
      liftIO (takeMVar uMap) >>=
      (\uMap' -> return (Map.lookup id' uMap') >>=
        (\user -> case user of
            Just _ ->
              Control.Distributed.Process.send server'
                  (cPID,ProcessedMsg n "User information updated") >>
              liftIO (putMVar uMap (Map.insert id' rAcc uMap'))
            _ ->
              Control.Distributed.Process.send server'
                  (cPID,ProcessedMsg n "User information updated") >>
              liftIO (putMVar uMap uMap')))))

-- |Handles a ChangeUsersPasswordMsg by checking user permissions and then passing
-- the message on if allowed.
handleChangeUsersPassword :: MVar ProcessId -> MVar ConnMap -> MVar UserMap ->
  ResourceNode -> (ConnectionId,ChangeUsersPasswordMsg) -> Process ()
handleChangeUsersPassword server cMap uMap rNode msg@(cPID,ChangeUsersPasswordMsg n _ _) =
  void $ spawnLocal
  (liftIO (readMVar server) >>=
   (\server' ->
      fmap (Map.lookup cPID) (liftIO (readMVar cMap)) >>=
      (\cMap' -> case cMap' of
          Just uid ->
            fmap (Map.lookup uid) (liftIO (readMVar uMap)) >>=
            (\user -> case user of
                Just True ->
                  liftIO (readMVar rNode) >>=
                  (`Control.Distributed.Process.send` msg)
                Just False -> Control.Distributed.Process.send server'
                                  (cPID,ProcessedMsg n "You do not have root access")
                _ -> failed server')
          _ -> failed server')))
  where
    failed server' = Control.Distributed.Process.send server'
             (cPID,ProcessedMsg n "Please Login")
