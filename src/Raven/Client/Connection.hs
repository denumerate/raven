module Raven.Client.Connection
  ( listenAtEnd
  , sendReq
  )where

import Network.Transport
import Control.Concurrent

import Graphics.UI.Gtk hiding (Action, backspace)
import Codec.Picture

import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.Vector (Vector)
import qualified Data.Vector as V
import Text.Read

import Control.Monad (void)

-- |Listens at endpoint and handles events
listenAtEnd :: EndPoint -> TextBuffer -> MVar (Vector TextBuffer) -> IO ()
listenAtEnd end connbuf workbufs =
  receive end >>=
  (\event -> case event of
      ConnectionClosed _ ->
        textBufferSetText connbuf "Connection Closed" >>
        listenAtEnd end connbuf workbufs
      EndPointClosed ->
        textBufferSetText connbuf "Error: Endpoint Closed" >>
        listenAtEnd end connbuf workbufs
      ErrorEvent (TransportError _ err) ->
        textBufferSetText connbuf
             ("Connection Error: " ++ err) >>
        listenAtEnd end connbuf workbufs
      Received _ [val] -> modBuf workbufs val >>
        listenAtEnd end connbuf workbufs
      Received _ vals -> buildImg vals >>
        listenAtEnd end connbuf workbufs
      _ -> listenAtEnd end connbuf workbufs)

-- |Sends a request to the server.
-- Needs the connection, the line number, and the message
sendReq :: Connection -> Int -> ByteString -> IO ()
sendReq conn n val = void $ send conn [B.pack $ show n,B.pack " ",val] --error possible

-- |updates the work buffer
modBuf :: MVar (Vector TextBuffer) -> ByteString -> IO ()
modBuf bufs str = let wds = B.words str in
  case (readMaybe (B.unpack (head wds)) :: Maybe Int) of
    Just n -> readMVar bufs >>=
      (\bufs' -> let buf = bufs' V.! n in
          textBufferGetEndIter buf >>=
          (\iter ->
             textBufferInsertByteString buf iter (B.unwords (tail wds))))
    _ -> return ()

buildImg :: [ByteString] -> IO ()
buildImg vals = let vals' = B.concat vals
  in case decodeImage (B.tail (B.dropWhile (/= ' ') vals')) of
       Left str -> putStrLn str --log this
       Right img ->
         savePngImage (".raven/client/plots/" ++
                       B.unpack (B.takeWhile (/= ' ') vals') ++ ".png") img
