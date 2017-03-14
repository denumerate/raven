module Raven.Client.Connection
  ( listenAtEnd
  )where

import Network.Transport
import Graphics.UI.Gtk hiding (Action, backspace)

listenAtEnd :: EndPoint -> TextBuffer -> IO ()
listenAtEnd end buf =
  receive end >>=
  (\event -> case event of
      ConnectionClosed _ ->
        textBufferSetText buf "Connection Closed" >>
        listenAtEnd end buf
      EndPointClosed ->
        textBufferSetText buf "Error: Endpoint Closed" >>
        listenAtEnd end buf
      ErrorEvent (TransportError _ err) ->
        textBufferSetText buf
             ("Connection Error: " ++ err) >>
        listenAtEnd end buf
      _ -> listenAtEnd end buf)
