module Raven.Server.ResourceNode
  ( ResourceNode
  , newResourceNode
  , cleanResourceNode
  )where

import Network.Transport
import Control.Distributed.Process
import Control.Distributed.Process.Node
import Control.Concurrent
import Control.Monad(forever,void)
import qualified Database.MongoDB as DB

import System.IO
import System.Directory

import Raven.Server.NodeMsgs
import Raven.DataBase

-- |Stores the ProcessId of the listen process
type ResourceNode = MVar ProcessId

-- |Builds and returns the node.
-- Needs the transport layer, server address, and database address.
newResourceNode :: Transport -> MVar ProcessId -> String -> IO ResourceNode
newResourceNode trans server dbAddr =
  getHomeDirectory >>=
  setCurrentDirectory >>
  createDirectoryIfMissing True ".raven/plots" >>
  openFile ".raven/log" AppendMode >>=
  (\logH -> hSetBuffering logH (BlockBuffering Nothing) >>
  DB.connect (DB.readHostPort dbAddr) >>=
    (\db -> forkIO (ensureUsers db) >>
      newLocalNode trans initRemoteTable >>=
      (\node -> newEmptyMVar >>=
        (\pid -> runProcess node
          (spawnLocal (forever (receiveWait
                                 [ match (handleLog logH)
                                 , match (handleLogin server db)
                                 , match (handleKill logH db)
                                 , match (handleAllUsers server db)
                                 , match (handleAddUser server db)
                                 , match (handleDeleteUser db server)
                                 , match (handleChangeRootAccess db server)
                                 , match (handleChangeUsersPassword server db)
                                 , matchUnknown (catchAllMsgs' pid "ResourceNode")
                                 ])) >>=
            liftIO . putMVar pid) >>
          return pid))))

-- |Tells the listening process on a resourceNode to exit
cleanResourceNode :: ResourceNode -> Process ()
cleanResourceNode self = liftIO (readMVar self) >>=
  (`exit` "Cleaning ResourceNode")

-- |Handles a log message by printing it to the supplied handle,
-- should be the log file.
handleLog :: Handle -> LogMsg -> Process ()
handleLog h (LogMsg msg pid time) =
  liftIO (hPutStrLn h ("[" ++ show pid ++ ": " ++ time ++ "] " ++ msg))

-- |Handles a kill message by killing the node and closing (and flushing) the buffer.
handleKill :: Handle -> DB.Pipe -> KillMsg -> Process ()
handleKill h p _ =
  liftIO cleanFiles >>
  liftIO (DB.close p) >>
  liftIO (hClose h) >>
  getSelfPid >>= (`exit` "Clean")

-- |Cleans up all excess files
cleanFiles :: IO ()
cleanFiles = removeDirectoryRecursive ".raven/plots"

-- |Handles a login message by sending a request to the database that makes sure
-- the user exists and then makes sure that the returned data has the right information.
-- Also sends a message to the connection node regarding success/failure
handleLogin :: MVar ProcessId -> DB.Pipe -> (ConnectionId,LoginMsg) -> Process ()
handleLogin server p (cPID,LoginMsg n name pass) = void $ spawnLocal
  (liftIO (readMVar server) >>=
   (\server' -> liftIO (checkUser p name pass) >>=
     (\ret -> case ret of
         (Just (Left info)) ->
           Control.Distributed.Process.send server'
              (cPID,LoginSucMsg info) >>
           Control.Distributed.Process.send server'
              (cPID,ProcessedMsg n "Login Successful")
         (Just (Right err)) -> buildLogMsg err >>=
           Control.Distributed.Process.send server' >>
           Control.Distributed.Process.send server'
              (cPID,ProcessedMsg n "Login Failed")
         _ -> Control.Distributed.Process.send server'
           (cPID,ProcessedMsg n "Login Failed"))))

-- |Handles an allUsers message by asking the database for all users, formatting them,
-- and sending them back to the connection node.
handleAllUsers :: MVar ProcessId -> DB.Pipe -> (ConnectionId,AllUsersMsg) ->
  Process ()
handleAllUsers server p (cPID,AllUsersMsg n) = void $ spawnLocal
  (liftIO (readMVar server)>>=
   (\server' ->
      liftIO (getAllUsers p) >>=
      (\res ->
         Control.Distributed.Process.send server' (cPID,ProcessedMsg n res))))

-- |Handles an addUser message by asking the database to add a user and sending
-- the result back to the connNode.
handleAddUser :: MVar ProcessId -> DB.Pipe -> (ConnectionId,AddUserMsg) ->
  Process ()
handleAddUser server p (cPID,AddUserMsg n name pswd rAcc) = void $ spawnLocal
  (liftIO (readMVar server) >>=
   (\server' ->
      liftIO (addUser p name pswd rAcc) >>=
      (\res ->
         Control.Distributed.Process.send server' (cPID,ProcessedMsg n res))))

-- |Handles a delete user message by asking the database to remove the user.
-- If successful, tell the severnode, if not, tells connNode
handleDeleteUser :: DB.Pipe -> MVar ProcessId -> (ConnectionId,DeleteUserMsg) ->
  Process ()
handleDeleteUser p server (cPID,DeleteUserMsg n name) = void $ spawnLocal
  (liftIO (readMVar server) >>=
   (\server' ->
      liftIO (deleteUser p name) >>=
      (\out -> case out of
          Just id' ->
            Control.Distributed.Process.send server'
               (cPID,DeleteUserSuccMsg n id')
          _ -> Control.Distributed.Process.send server'
            (cPID,ProcessedMsg n "User not found"))))

-- |Handles an updateusersaccess message by asking the database to find and
-- update the user.
-- If successful, tell the severnode, if not, tells connNode
handleChangeRootAccess :: DB.Pipe -> MVar ProcessId ->
  (ConnectionId,ChangeRootAccessMsg) -> Process ()
handleChangeRootAccess p server (cPID,ChangeRootAccessMsg n name rAcc) =
  void $ spawnLocal
  (liftIO (readMVar server) >>=
   (\server' ->
      liftIO (updateUsersAccess p name rAcc) >>=
      (\out -> case out of
          Just id' ->
            Control.Distributed.Process.send server'
               (cPID,RootAccessChangedMsg n id' rAcc)
          _ -> Control.Distributed.Process.send server'
            (cPID,ProcessedMsg n "User not found"))))

-- |Handles a ChangeUsersPasswordMsg by asking the database to find and
-- update the user.
-- Tells the connNode the result.
handleChangeUsersPassword :: MVar ProcessId -> DB.Pipe ->
  (ConnectionId, ChangeUsersPasswordMsg) -> Process ()
handleChangeUsersPassword server p (cPID, ChangeUsersPasswordMsg n name pswd) =
  void $ spawnLocal
  (liftIO (readMVar server) >>=
   (\server' ->
      liftIO (updateUsersPassword p name pswd) >>=
      (\out -> case out of
          Just _ ->
            Control.Distributed.Process.send server'
               (cPID,ProcessedMsg n "Password Changed")
          _ -> Control.Distributed.Process.send server'
               (cPID,ProcessedMsg n "User not found"))))
