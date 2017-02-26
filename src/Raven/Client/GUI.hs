{-# LANGUAGE OverloadedStrings #-}
module Raven.Client.GUI
  ( guiMain
  )where

import Network.Transport

import Brick.Main
import Brick.Types
import Brick.Widgets.Core
import Brick.AttrMap
import Brick.BChan
import Graphics.Vty

import Control.Monad.State
import Control.Concurrent

import Data.Text (Text)
import qualified Data.Text as Text

buffersize = 25

-- |Builds and runs terminal ui
guiMain :: Connection -> EndPoint -> IO ()
guiMain conn end = let app =
                         (App
                           { appDraw = myDraw
                           , appChooseCursor = neverShowCursor
                           , appHandleEvent = myHandle
                           , appStartEvent = return
                           , appAttrMap = const $ attrMap Graphics.Vty.defAttr []
                           } :: App Text Text ())
                   in forkIO (listenAtEnd end) >>
                      newBChan buffersize >>=
                      (\chan ->
                         customMain (mkVty defaultConfig) (Just chan) app
                         ((Text.pack . show . address) end)) >>
                      return ()

-- |All widgets
myDraw :: Text -> [Widget n]
myDraw s = [connStatus s]

-- |Connection Status Widget
connStatus :: Text -> Widget n
connStatus s = if Text.isPrefixOf "Connection Error:" s
               then padAll 2 $ txt s
               else padAll 2 $ txt $ Text.concat
                    ["Connection: ",s]

-- |Handle events
myHandle :: s -> BrickEvent n e -> EventM n (Next s)
myHandle s (AppEvent e) = halt s
myHandle s _ = continue s

-- |Listen for and handle events from the endpoint
listenAtEnd :: EndPoint -> IO ()
listenAtEnd end = receive end >>=
  (\event -> case event of
      Received _ info -> forkIO (print info) >>
        listenAtEnd end
      ConnectionClosed _ -> putStrLn "Connection Closed (Client)"
      EndPointClosed -> putStrLn "EndPoint Closed (Client)"
      _ -> listenAtEnd end)
