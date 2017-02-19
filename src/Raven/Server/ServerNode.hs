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

-- |Builds a server node and all internal processes
newServerNode :: Transport -> EndPoint -> MVar Bool -> IO ()
newServerNode trans end running = newLocalNode trans initRemoteTable >>=
  (\node -> putStrLn ("Server established at " ++ (show . address) end) >>
  newREPLNode trans >>=
    (\replNode -> newEmptyMVar >>=
      (\pid ->
          runProcess node
          (spawnLocal (liftIO (listenAtEnd trans pid end running Map.empty)) >>
           spawnLocal (forever (receiveWait
                                 [match (handleREPL replNode)])) >>=
           liftIO . putMVar pid)) >>
    forkIO (cleanNodes node replNode running) >>
    return ()))

-- |Listen for and handle events from the endpoint.
-- The pid is the id of the node's listening process
listenAtEnd :: Transport -> MVar ProcessId -> EndPoint -> MVar Bool ->
  Map ConnectionId (MVar ConnNode) -> IO ()
listenAtEnd trans pid end running conns = readMVar running >>=
  (\running' -> if running'
    then receive end >>=
         (\event -> case event of
             ConnectionOpened cid reliabilty adrs -> newEmptyMVar >>=
               (\cNode -> forkIO
                 (Network.Transport.connect end adrs reliabilty defaultConnectHints >>=
                  (\conn -> case conn of
                      Right conn' -> newConnNode trans conn' >>=
                        putMVar cNode
                      _ -> putStrLn "Connection failed")) >> --move to log
                 listenAtEnd trans pid end running (Map.insert cid cNode conns))
             Received cid info -> forkIO
               (case Map.lookup cid conns of
                  Just conn -> readMVar conn >>=
                    handleReceived pid info >>
                    return ()
                  Nothing -> putStrLn "Connection not found") >> --move to log
                    listenAtEnd trans pid end running conns
             ConnectionClosed cid -> listenAtEnd trans pid end running $
               Map.delete cid conns
             EndPointClosed -> putStrLn "Server Closing" --log?
             _ -> putStrLn "Missing case") --move to log
    else putStrLn "Server Closing" >> --log?
         Map.foldl' clean (return ()) conns >>
         return ())
  where
    clean acc val = acc >>
      readMVar val >>=
      cleanConnNode

-- |Handles a REPLMsg
handleREPL :: REPLNode -> (ProcessId,REPLMsg) -> Process ()
handleREPL replNode msg = liftIO (readMVar replNode) >>=
  (\replNode' -> Control.Distributed.Process.send replNode' msg)

-- |Clean up all connected nodes if the MVar is false (not running anymore).
-- Note that connNodes are cleaned up by the listen process
cleanNodes :: LocalNode -> REPLNode -> MVar Bool -> IO ()
cleanNodes sNode rNode running = readMVar running >>=
  (\running' -> if running'
    then cleanNodes sNode rNode running
    else runProcess sNode
         (cleanREPLNode rNode))
