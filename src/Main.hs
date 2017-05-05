{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Main where

import Network.Transport
import Network.Info
import System.Console.GetOpt
import System.Environment
import Control.Concurrent
import Control.Concurrent.Async
import qualified Data.ByteString.Char8 as B

import Raven.Server
import Raven.Client

data Options = Options
  { server :: Bool
  , serverPort :: String
  , client :: Maybe String
  , clientPort :: String
  , help :: Bool
  , version :: Bool
  , ip :: String
  , dbip :: String
  , dbport :: Maybe String
  }

main :: IO ()
main = getIp >>=
  (\ip' -> getArgs >>=
    (\args -> case getOpt RequireOrder options args of
                (actions,_,[]) ->
                  foldl (>>=) (return (startOptions ip')) actions >>=
                  (\Options{..} -> case (version,help,server,client) of
                      (True,_,_,_) -> putStrLn $ "Raven version " ++ ver
                      (_,True,_,_) -> putStrLn $ usageInfo header options
                      (_,_,True,Just address) ->
                        forkIO (initServer ip serverPort (buildDBString dbip dbport)) >>
                        initClient ip clientPort
                                   (EndPointAddress (B.pack address))
                      (_,_,_,Just address) -> initClient ip clientPort
                                   (EndPointAddress (B.pack address))
                      _ -> initServer "127.0.0.1" serverPort
                                   (buildDBString dbip dbport))
                (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))))
  where
    header = "Usage: raven [OPTION..]"
    ver = "0.1.10"

getIp :: IO String
getIp =
  fmap (filter (\ip -> ip /= "0.0.0.0" && ip /= "127.0.0.1") .
         map (show . ipv4)) getNetworkInterfaces >>=
  (\ips -> if null ips then return "127.0.0.1" else return (head ips))

buildDBString :: String -> Maybe String -> String
buildDBString ip (Just port) = ip ++ ":" ++ port
buildDBString ip _ = ip

startOptions :: String -> Options
startOptions address = Options
  { server = True
  , serverPort = "4444"
  , client = Just $ address ++ ":4444:0"
  , clientPort = "1234"
  , help = False
  , version = False
  , ip = address
  , dbip = "127.0.0.1"
  , dbport = Nothing
  }

options :: [OptDescr (Options -> IO Options)]
options =
  [ Option "s" ["serverOnly"]
    (NoArg (\opt -> return opt{client = Nothing}))
    "Stops the Client from running"
  , Option "c" ["clientOnly"]
    (OptArg (\arg opt -> case arg of
                           Nothing -> return opt{server = False}
                           Just arg' -> return opt
                                        { server = False
                                        , client = Just arg'
                                        })
      "Passes the server address")
    "Stops the Sever from running"
  , Option "o" ["clientPort"]
    (ReqArg (\arg opt -> return opt{clientPort = arg})
     "Port Number")
    "Sets the port the client uses, default is 1234"
  , Option "p" ["serverPort"]
    (ReqArg (\arg opt@Options{..} ->
                return opt
                { serverPort = arg
                , client = client >>= return (Just ("127.0.0.1:" ++ arg ++ ":0"))
                })
      "Port Number")
    "Sets the port the client uses, default is 4444"
  , Option "h" ["help"]
    (NoArg (\opt -> return opt{help = True}))
    "Usage help"
  , Option "v" ["version"]
    (NoArg (\opt -> return opt{version = True}))
    "Print version info"
  , Option "l" ["localhost"]
    (NoArg (\opt@Options{..} -> return opt
             { ip = "127.0.0.1"
             , client = client >>=
               return (Just ("127.0.0.1:" ++ serverPort ++ ":0"))
             }))
    "Sets the ip to localhost"
  , Option "I" ["database ip address"]
    (ReqArg (\arg opt@Options{..} ->
               return opt
               { dbip = arg
               })
     "Database IP Address")
    "Sets the database's ip address (default is 127.0.0.1)"
  , Option "P" ["database port number"]
    (ReqArg (\arg opt@Options{..} ->
               return opt
               { dbport = Just arg
               })
     "Database Port Number")
    "Set the database port number, default is the mongoDB default"
  ]
