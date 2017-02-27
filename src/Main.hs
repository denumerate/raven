{-# LANGUAGE OverloadedStrings
, RecordWildCards #-}
module Main where

import Network.Transport
import System.Console.GetOpt
import System.Environment
import Control.Concurrent

import Raven.Server
import Raven.Client

data Flag = ServerOnly
          | ClientOnly
          | SPort String
          | CPort String

data Options = Options
  { server :: Bool
  , serverPort :: String
  , client :: Bool
  , clientPort :: String
  }

main :: IO ()
main = getArgs >>=
  (\args -> let (actions, _, _) = getOpt RequireOrder options args in
      foldl (>>=) (return startOptions) actions >>=
      (\Options{..} -> case (server,client) of
          (True,True) ->
            forkIO (initClient "127.0.0.1" clientPort
                    (EndPointAddress "127.0.0.1:4444:0")) >>
            initServer "127.0.0.1" serverPort
          (_,True) -> initClient "127.0.0.1" clientPort
                      (EndPointAddress "127.0.0.1:4444:0")
          _ -> initServer "127.0.0.1" serverPort))

startOptions :: Options
startOptions = Options
  { server = True
  , serverPort = "4444"
  , client = True
  , clientPort = "1234"
  }

options :: [OptDescr (Options -> IO Options)]
options =
  [ Option "s" ["serverOnly"]
    (NoArg (\opt -> return opt{client = False}))
    "Stops the Client from running"
  , Option "c" ["clientOnly"]
    (NoArg (\opt -> return opt{server = False}))
    "Stops the Sever from running"
  , Option "l" ["clientPort"]
    (ReqArg (\arg opt -> return opt{clientPort = arg})
     "Port Number")
    "Sets the port the client uses, default is 1234"
  , Option "p" ["serverPort"]
    (ReqArg (\arg opt -> return opt{serverPort = arg})
      "Port Number")
    "Sets the port the client uses, default is 4444"
  ]
