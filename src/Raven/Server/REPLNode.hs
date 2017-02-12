module Raven.Server.REPLNode
  ( REPLNode
  , newREPLNode
  )where

import Network.Transport
import Control.Distributed.Process
import Control.Distributed.Process.Node
import Control.Concurrent
import Control.Monad(forever)

import Language.Haskell.Interpreter

import Raven.Server.NodeMsgs
import Raven.REPL

-- |Stores the id of the listen Process
type REPLNode = MVar ProcessId

-- |Builds and returns the node to handle the RELP
-- Needs the transport layer
newREPLNode :: Transport -> IO REPLNode
newREPLNode trans = newEmptyMVar >>=
  (\pid -> newEmptyMVar >>=
    (\interpS -> newLocalNode trans initRemoteTable >>=
      (\replNode ->
         runProcess replNode
        (spawnLocal (forever (receiveWait [match (runREPL interpS)])) >>=
         liftIO . putMVar pid) >>
        putMVar interpS initREPL >>
        return pid)))

-- |matches to a REPLmsg and runs the repl on the sent string.
-- Sends back a ProcessedMsg to the connection
runREPL :: MVar (Interpreter ()) -> (ProcessId,REPLMsg) -> Process ()
runREPL interpS (pid,(REPLMsg value)) = spawnLocal
  (liftIO (readMVar interpS) >>=
   (\interpS' -> liftIO (interp interpS' value)) >>=
   Control.Distributed.Process.send pid . ProcessedMsg) >>
  return ()
