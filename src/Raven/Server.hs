module Raven.Server
  ( initServer
  ) where

import Network
import Network.Transport
import Network.Transport.TCP

import Raven.Server.ServerNode

-- |Start the server and listen for connections at the supplied ip:port number.
-- Third value is the database address.
initServer :: String -> String -> String -> IO ()
initServer ip portNum db = withSocketsDo $
  createTransport ip portNum defaultTCPParameters >>=
  (\trans -> case trans of
      Right trans' -> newEndPoint trans' >>=
        (\end -> case end of
            Right end' -> newServerNode trans' end' db
            _ -> putStrLn "Endpoint not initialized, Server Failed")
      _ -> putStrLn "Transport not initialized, Server Failed")
