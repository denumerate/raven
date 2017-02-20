{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Concurrent
import Network.Transport
import Control.Exception

import Raven.Server
import Raven.Client

main :: IO ()
main = initServer "127.0.0.1" "4444" >>=
       (\t -> onCtrlC (initClient "127.0.0.1" "1234" (EndPointAddress "127.0.0.1:4444:0"))
       (putStrLn "ldskfj" >>
        let Just t' = t in closeTransport t'))

onCtrlC :: IO a -> IO () -> IO a
onCtrlC p q = catchJust isUserInterrupt p (const $ q >> p `onCtrlC` q)
  where
    isUserInterrupt :: AsyncException -> Maybe ()
    isUserInterrupt UserInterrupt = Just ()
    isUserInterrupt _             = Nothing
