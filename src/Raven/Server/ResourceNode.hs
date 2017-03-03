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

import Raven.Server.NodeMsgs
import Raven.DataBase

-- |Stores the ProcessId of the listen process
type ResourceNode = MVar ProcessId

-- |Builds and returns the node.
-- Needs the transport layer and Maybe a logging level (Standard is default)
newResourceNode :: Transport -> MVar ProcessId -> IO ResourceNode
newResourceNode trans server =
  getHomeDirectory >>=
  setCurrentDirectory >>
  createDirectoryIfMissing False ".raven" >>
  openFile ".raven/log" AppendMode >>=
  (\logH -> hSetBuffering logH (BlockBuffering Nothing) >>
  DB.connect (DB.host "127.0.0.1") >>=
    (\db -> forkIO (ensureUsers db) >>
      newLocalNode trans initRemoteTable >>=
      (\node -> newEmptyMVar >>=
        (\pid -> runProcess node
          (spawnLocal (forever (receiveWait
                                 [ match (handleLog logH)
                                 , match (handleLogin server db)
                                 , match (handleKill logH)
                                 , matchUnknown (catchAllMsgs' pid "ResourceNode")
                                 ])) >>=
            liftIO . putMVar pid) >>
          return pid))))

-- |Tells the listening process on a resourceNode to exit
cleanResourceNode :: ResourceNode -> Process ()
cleanResourceNode self = liftIO (readMVar self) >>=
  (`exit` "Cleaning ResourceNode")

-- |Handles a log message
handleLog :: Handle -> LogMsg -> Process ()
handleLog h (LogMsg msg pid time) =
  liftIO (hPutStrLn h ("[" ++ show pid ++ ": " ++ time ++ "] " ++ msg))

-- |Handles a kill message
handleKill :: Handle -> KillMsg -> Process ()
handleKill h _ =
  liftIO (hClose h) >>
  getSelfPid >>= (`exit` "Clean")

handleLogin :: MVar ProcessId -> DB.Pipe -> (ProcessId,LoginMsg) -> Process ()
handleLogin server p (cPID,LoginMsg n name pass) = liftIO (readMVar server) >>=
  (\server' -> spawnLocal
    (liftIO (checkUser p name pass) >>=
     (\ret -> case ret of
         (Just (Left info)) ->
           Control.Distributed.Process.send server' (cPID,LoginSucMsg info) >>
           Control.Distributed.Process.send cPID (ProcessedMsg n "Login Successful")
         (Just (Right err)) -> buildLogMsg err >>=
           Control.Distributed.Process.send server'
         _ -> Control.Distributed.Process.send cPID (ProcessedMsg n "Login Failed"))) >>
    return ())
