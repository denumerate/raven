module Raven.Client
  ( initClient
  )where

import Network
import Network.Transport
import Network.Transport.TCP
import Control.Concurrent
import Control.Monad (forever)

import qualified Data.ByteString.Char8 as B

-- |Start the client and listen for connections at the supplied ip:port number
-- then connects to the server
-- Returns the address of the client (if successful)
initClient :: String -> String -> EndPointAddress -> IO ()
initClient ip portNum serverAdrs = withSocketsDo $
  createTransport ip portNum defaultTCPParameters >>=
  (\trans -> case trans of
      Right trans' -> newEndPoint trans' >>=
        (\end -> case end of
            Right end' -> connect end' serverAdrs ReliableOrdered defaultConnectHints >>=
              (\conn -> case conn of
                  Right conn' ->
                    forkIO (listenAtEnd end') >>
                    forever (getAndSendLine conn') >>
                    return ()
                  _ -> putStrLn "Connection Refused, Client Failed" >> --move to log
                    return ())
            _ -> putStrLn "Endpoint not initialized, Client Failed" >> --move to log
                 return ())
      _ -> putStrLn "Transport not initialized, Client Failed" >> --move to log
        return ())

-- |Listen for and handle events from the endpoint
listenAtEnd :: EndPoint -> IO ()
listenAtEnd end = receive end >>=
  (\event -> case event of
      Received _ info -> forkIO (print info) >>
        listenAtEnd end
      ConnectionClosed _ -> putStrLn "Connection Closed"
      EndPointClosed -> putStrLn "EndPoint Closed"
      _ -> listenAtEnd end)

getAndSendLine :: Connection -> IO ()
getAndSendLine conn = getLine >>=
  (\line -> Network.Transport.send conn [B.pack line]) >>
  return ()
