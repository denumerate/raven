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

import System.IO
import System.Directory

import Raven.Server.NodeMsgs

-- |Stores the ProcessId of the listen process
type ResourceNode = MVar ProcessId

-- |Builds and returns the node.
-- Needs the transport layer and Maybe a logging level (Standard is default)
newResourceNode :: Transport -> IO ResourceNode
newResourceNode trans = doesDirectoryExist "~/.raven" >>=
  (\dirBool -> if dirBool then return () else createDirectory "~/.raven") >>
  openFile "~/.raven/log" AppendMode >>=
  (\logH -> hSetBuffering logH (BlockBuffering Nothing) >>
    newLocalNode trans initRemoteTable >>=
    (\node -> newEmptyMVar >>=
      (\pid -> runProcess node
               (spawnLocal (forever (receiveWait
                                     [ match (handleLog logH)
                                     , match (handleKill logH)
                                     , matchUnknown (catchAllMsgs' pid "ResourceNode")
                                     ])) >>=
                liftIO . putMVar pid) >>
               return pid)))

-- |Tells the listening process on a resourceNode to exit
cleanResourceNode :: ResourceNode -> Process ()
cleanResourceNode self = liftIO (readMVar self) >>=
  (`exit` "Cleaning ResourceNode")

-- |Handles a log message
handleLog :: Handle -> LogMsg -> Process ()
handleLog h (LogMsg msg pid time) = liftIO $ hPutStrLn h $
  "[" ++ show pid ++ ": " ++ time ++ "] " ++ msg

-- |Handles a kill message
handleKill :: Handle -> KillMsg -> Process ()
handleKill h _ = liftIO (hClose h) >>
  getSelfPid >>= (`exit` "Clean")
