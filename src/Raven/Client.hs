module Raven.Client
  ( initClient
  )where

import Network
import Network.Transport
import Network.Transport.TCP

import Raven.Client.GUI

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
                    guiMain conn' end' serverAdrs >>
                    closeTransport trans'
                  _ -> putStrLn "Connection Refused, Client Failed" >> --move to log
                    return ())
            _ -> putStrLn "Endpoint not initialized, Client Failed" >> --move to log
                 return ())
      _ -> putStrLn "Transport not initialized, Client Failed" >> --move to log
        return ())
