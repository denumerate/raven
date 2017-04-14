module Raven.Client.Connection
  ( listenAtEnd
  , sendReq
  )where

import Network.Transport
import Graphics.UI.Gtk hiding (Action, backspace)

import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B

-- |Listens at endpoint and handles events
listenAtEnd :: EndPoint -> TextBuffer -> IO ()
listenAtEnd end connbuf =
  receive end >>=
  (\event -> case event of
      ConnectionClosed _ ->
        textBufferSetText connbuf "Connection Closed" >>
        listenAtEnd end connbuf
      EndPointClosed ->
        textBufferSetText connbuf "Error: Endpoint Closed" >>
        listenAtEnd end connbuf
      ErrorEvent (TransportError _ err) ->
        textBufferSetText connbuf
             ("Connection Error: " ++ err) >>
        listenAtEnd end connbuf
      Received _ val -> print val >>
        listenAtEnd end connbuf
      _ -> listenAtEnd end connbuf)

-- |Sends a request to the server.
-- Needs the connection, the line number, and the message
sendReq :: Connection -> Int -> ByteString -> IO ()
sendReq conn n val = send conn [B.pack $ show n,B.pack " ",val] >> --error possible
  return ()
