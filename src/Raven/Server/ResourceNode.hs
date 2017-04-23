module Raven.Server.ResourceNode
  ( ResourceNode
  , newResourceNode
  , cleanResourceNode
  )where

import Network.Transport
import Control.Distributed.Process
import Control.Distributed.Process.Node
import Control.Concurrent
import Control.Monad(forever)
import qualified Database.MongoDB as DB

import System.IO
import System.Directory
import Codec.Picture

import qualified Data.ByteString.Lazy as B

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
                                 , match (handleAllUsers db)
                                 , match (handleAddUser db)
                                 , match (handleDeleteUser db server)
                                 , match (handleChangeRootAccess db server)
                                 , match (handleChangeUsersPassword db)
                                 , match handlePlotDone
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
handleLogin :: MVar ProcessId -> DB.Pipe -> (ProcessId,LoginMsg) -> Process ()
handleLogin server p (cPID,LoginMsg n name pass) = spawnLocal
  (liftIO (readMVar server) >>=
   (\server' -> liftIO (checkUser p name pass) >>=
     (\ret -> case ret of
         (Just (Left info)) ->
           Control.Distributed.Process.send server' (cPID,LoginSucMsg info) >>
           Control.Distributed.Process.send cPID (ProcessedMsg n "Login Successful")
         (Just (Right err)) -> buildLogMsg err >>=
           Control.Distributed.Process.send server' >>
           Control.Distributed.Process.send cPID (ProcessedMsg n "Login Failed")
         _ -> Control.Distributed.Process.send cPID (ProcessedMsg n "Login Failed")))) >>
    return ()

-- |Handles an allUsers message by asking the database for all users, formatting them,
-- and sending them back to the connection node.
handleAllUsers :: DB.Pipe -> (ProcessId,AllUsersMsg) -> Process ()
handleAllUsers p (cPID,AllUsersMsg n) = spawnLocal
  (liftIO (getAllUsers p) >>=
   Control.Distributed.Process.send cPID . ProcessedMsg n) >>
  return ()

-- |Handles an addUser message by asking the database to add a user and sending
-- the result back to the connNode.
handleAddUser :: DB.Pipe -> (ProcessId,AddUserMsg) -> Process ()
handleAddUser p (cPID,AddUserMsg n name pswd rAcc) = spawnLocal
  (liftIO (addUser p name pswd rAcc) >>=
   Control.Distributed.Process.send cPID . ProcessedMsg n) >>
  return ()

-- |Handles a delete user message by asking the database to remove the user.
-- If successful, tell the severnode, if not, tells connNode
handleDeleteUser :: DB.Pipe -> MVar ProcessId -> (ProcessId,DeleteUserMsg) ->
  Process ()
handleDeleteUser p server (cPID,DeleteUserMsg n name) = spawnLocal
  (liftIO (deleteUser p name) >>=
   (\out -> case out of
       Just id' -> liftIO (readMVar server) >>=
         (`Control.Distributed.Process.send` (cPID,DeleteUserSuccMsg n id'))
       _ -> Control.Distributed.Process.send cPID
         (ProcessedMsg n "User not found"))) >>
  return ()

-- |Handles an updateusersaccess message by asking the database to find and
-- update the user.
-- If successful, tell the severnode, if not, tells connNode
handleChangeRootAccess :: DB.Pipe -> MVar ProcessId ->
  (ProcessId,ChangeRootAccessMsg) -> Process ()
handleChangeRootAccess p server (cPID,ChangeRootAccessMsg n name rAcc) =
  spawnLocal
  (liftIO (updateUsersAccess p name rAcc) >>=
    (\out -> case out of
        Just id' -> liftIO (readMVar server) >>=
          (`Control.Distributed.Process.send` (cPID,RootAccessChangedMsg n id' rAcc))
        _ -> Control.Distributed.Process.send cPID
          (ProcessedMsg n "User not found"))) >>
  return ()

-- |Handles a ChangeUsersPasswordMsg by asking the database to find and
-- update the user.
-- Tells the connNode the result.
handleChangeUsersPassword :: DB.Pipe -> (ProcessId, ChangeUsersPasswordMsg) ->
  Process ()
handleChangeUsersPassword p (cPID, ChangeUsersPasswordMsg n name pswd) =
  spawnLocal
  (liftIO (updateUsersPassword p name pswd) >>=
    (\out -> case out of
        Just _ ->
          Control.Distributed.Process.send cPID $
             ProcessedMsg n "Password Changed"
        _ -> Control.Distributed.Process.send cPID
          (ProcessedMsg n "User not found"))) >>
  return ()

-- |Handles a PlotDoneMsg by sending a serialized version of the image to the user
-- and then removing the file
handlePlotDone :: (ProcessId,PlotDoneMsg) -> Process ()
handlePlotDone (cPID,PlotDoneMsg n fname) =
  let fname' = ".raven/plots/" ++ fname
  in spawnLocal
     (liftIO (doesFileExist fname') >>=
       (\exists -> if exists
                   then liftIO (readImage fname') >>=
                        (\img -> case img of
                            Left str ->
                               Control.Distributed.Process.send cPID
                                 (ProcessedMsg n str)
                            Right img' -> case encodeDynamicBitmap img' of
                              Left str ->
                                Control.Distributed.Process.send cPID
                                 (ProcessedMsg n str)
                              Right bstring ->
                                Control.Distributed.Process.send cPID
                                 (ProcessedBSMsg n (B.toStrict bstring))) >>
                        liftIO (removeFile fname')
                   else Control.Distributed.Process.send cPID
                        (ProcessedMsg n "File error"))) >>
  return ()
