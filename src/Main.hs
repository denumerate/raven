{-# LANGUAGE OverloadedStrings #-}
module Main where

import Network.Transport
import System.Environment

import Raven.Server
import Raven.Client

main :: IO ()
main = getArgs >>=
  (\a -> if a == ["-s"]
         then initServer "127.0.0.1" "4444"
         else initClient "127.0.0.1" "1234" (EndPointAddress "127.0.0.1:4444:0"))
