{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Concurrent
import Network.Transport

import Raven.Server
import Raven.Client

main :: IO ()
main = forkIO (initServer "127.0.0.1" "4444") >>
     initClient "127.0.0.1" "1234"
             (EndPointAddress "127.0.0.1:4444:0")
