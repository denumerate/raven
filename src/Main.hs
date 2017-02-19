{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Concurrent
import Network.Transport
import Control.Exception

import Raven.Server
import Raven.Client

main :: IO ()
main = newMVar True >>=
       (\running ->  forkIO (initServer "127.0.0.1" "4444" running) >>
         initClient "127.0.0.1" "1234" (EndPointAddress "127.0.0.1:4444:0"))

onCtrlC :: IO a -> IO () -> IO a
p `onCtrlC` q = catchJust isUserInterrupt p (const $ q >> p `onCtrlC` q)
  where
    isUserInterrupt :: AsyncException -> Maybe ()
    isUserInterrupt UserInterrupt = Just ()
    isUserInterrupt _             = Nothing
