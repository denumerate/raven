module Raven.Server.REPLNode
  ( REPLNode
  , newREPLNode
  )where

import Network.Transport
import Control.Distributed.Process
import Control.Distributed.Process.Node
import Control.Concurrent
import Control.Monad(forever,void)

import Language.Haskell.Interpreter

import Raven.Server.NodeMsgs
import Raven.REPL

-- |Stores the id of the listen Process
type REPLNode = MVar ProcessId

-- |Builds and returns the node to handle the RELP
-- Needs the transport layer and server node
newREPLNode :: Transport -> MVar ProcessId -> IO REPLNode
newREPLNode trans server = newEmptyMVar >>=
  (\pid -> newEmptyMVar >>=
    (\interpS -> newLocalNode trans initRemoteTable >>=
      (\replNode ->
         runProcess replNode
        (spawnLocal (forever (receiveWait
                              [ match (runREPL interpS)
                              , match handleKill
                              , match (runPlot interpS server)
                              , matchUnknown (catchAllMsgs' server "REPLNode")
                              ])) >>=
         liftIO . putMVar pid) >>
        putMVar interpS initREPL >>
        return pid)))

-- |matches to a REPLmsg and runs the repl on the sent string.
-- Sends back a ProcessedMsg to the connection
runREPL :: MVar (Interpreter ()) -> (ProcessId,REPLMsg) -> Process ()
runREPL interpS (pid,REPLMsg n value) = void $ spawnLocal
  (liftIO (readMVar interpS) >>=
   (\interpS' -> liftIO (interp interpS' value)) >>=
    Control.Distributed.Process.send pid . ProcessedMsg n)

-- |Handles a kill message by killing the node
handleKill :: KillMsg -> Process ()
handleKill _ = getSelfPid >>=
  (`exit` "Clean")

-- |matches to a PlotMsg and runs the repl on the sent string.
-- If successful, sends off the PlotMsg to the resource node to get the image,
-- otherwise sends back errors.
runPlot :: MVar (Interpreter ()) -> MVar ProcessId -> (ProcessId,PlotMsg) -> Process ()
runPlot interpS server (pid,pm@(PlotMsg n _ _)) = void $ spawnLocal
  (liftIO (readMVar interpS) >>=
   (\interpS' -> liftIO (readMVar server) >>=
      (\server' -> liftIO (interpIO interpS' (buildPlotString pm)) >>=
        (\out -> case out of
            Left err ->
              (Control.Distributed.Process.send pid . ProcessedMsg n) err
            Right fname ->
              Control.Distributed.Process.send server'
                 (pid,PlotDoneMsg n fname)))))

-- |builds the string that is run by the interpreter from a PlotMsg
buildPlotString :: PlotMsg -> String
buildPlotString (PlotMsg _ ptype pnts) =
  "buildPlot $ plot $ " ++ ptype ++ " \"line\" [(" ++ pnts ++
  " :: [(Double,Double)])]"
