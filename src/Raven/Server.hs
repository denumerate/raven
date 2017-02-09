module Raven.Server
  ( initServer
  ) where

import Network
import System.IO
import Control.Monad(forever)
import Control.Concurrent(forkFinally)

import Raven.REPL

-- |Start the server and listen for connections at the supplied port number.
-- Creates a new thread to handle the connection
initServer :: Int -> IO ()
initServer portNum = withSocketsDo $
  listenOn (PortNumber (fromIntegral portNum)) >>=
  (\socket -> forever $ Network.accept socket >>=
    (\(handle,_,_) -> forkFinally (parrot handle) (\_ -> hClose handle)))

-- test function for init, to be replaced once I know what im doing
parrot :: Handle -> IO ()
parrot handle = hSetBuffering handle LineBuffering >>
  interp handle
