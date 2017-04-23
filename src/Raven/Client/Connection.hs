module Raven.Client.Connection
  ( listenAtEnd
  , sendReq
  )where

import Network.Transport
import Graphics.UI.Gtk hiding (Action, backspace)

import Codec.Picture

import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B

-- |Listens at endpoint and handles events
listenAtEnd :: EndPoint -> TextBuffer -> TextBuffer -> IO ()
listenAtEnd end connbuf workbuf =
  receive end >>=
  (\event -> case event of
      ConnectionClosed _ ->
        textBufferSetText connbuf "Connection Closed" >>
        listenAtEnd end connbuf workbuf
      EndPointClosed ->
        textBufferSetText connbuf "Error: Endpoint Closed" >>
        listenAtEnd end connbuf workbuf
      ErrorEvent (TransportError _ err) ->
        textBufferSetText connbuf
             ("Connection Error: " ++ err) >>
        listenAtEnd end connbuf workbuf
      Received _ [val] -> modBuf workbuf val >>
        listenAtEnd end connbuf workbuf
      Received _ vals -> buildImg vals >>
        listenAtEnd end connbuf workbuf
      _ -> listenAtEnd end connbuf workbuf)

-- |Sends a request to the server.
-- Needs the connection, the line number, and the message
sendReq :: Connection -> Int -> ByteString -> IO ()
sendReq conn n val = send conn [B.pack $ show n,B.pack " ",val] >> --error possible
  return ()

-- |updates the work buffer
modBuf :: TextBuffer -> ByteString -> IO ()
modBuf buf str =
  textBufferInsertByteStringAtCursor buf $
  B.concat [str,B.pack "\n"]

buildImg :: [ByteString] -> IO ()
buildImg vals = let vals' = B.concat vals
  in case decodeImage (B.drop 2 vals') of
       Left str -> putStrLn str --log this
       Right img ->
         savePngImage (".raven/client/plots/" ++ [B.head vals'] ++ ".png") img
