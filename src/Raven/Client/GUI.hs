{-# LANGUAGE OverloadedStrings #-}
module Raven.Client.GUI
  ( guiMain
  )where

import Network.Transport

import Brick.Main
import Brick.Types
import Brick.Widgets.Core
import Brick.Widgets.Border
import Brick.Widgets.Border.Style
import Brick.AttrMap
import Brick.BChan
import Graphics.Vty

import Control.Concurrent

import Data.Text (Text)
import qualified Data.Text as Text
import Data.List

import Raven.Client.Handler

data RName = WorkWindow
           | CName
  deriving (Eq,Ord,Show)
buffersize = 25

-- |Builds and runs terminal ui
guiMain :: Connection -> EndPoint -> IO ()
guiMain conn end = let app =
                         (App
                           { appDraw = myDraw
                           , appChooseCursor = showFirstCursor
                           , appHandleEvent = handler
                           , appStartEvent = return
                           , appAttrMap = const $ attrMap Graphics.Vty.defAttr []
                           } :: App (Text,[(Text,Text)]) Text RName)
                   in forkIO (listenAtEnd end) >>
                      newBChan buffersize >>=
                      (\chan ->
                         customMain (mkVty defaultConfig) (Just chan) app
                         ((Text.pack . show . address) end,[("","")])) >>
                      return ()

-- |All widgets
myDraw :: (Text,[(Text,Text)]) -> [Widget RName]
myDraw s = [ vBox [ hBorder
                  , connStatus s
                  , hBorder
                  , workWidget s
                  ]
           ]

-- |Connection Status Widget
connStatus :: (Text,[(Text,Text)]) -> Widget n
connStatus (s,_) = if Text.isPrefixOf "Connection Error:" s
               then withBorderStyle unicode $
                    padAll 1 $ txt s
               else withBorderStyle unicode $
                    padAll 1 $ txt $ Text.concat
                    ["Connection: ",s]

-- |Creates a work Widget
workWidget :: (Text,[(Text,Text)]) -> Widget RName
workWidget (_,dis) = viewport WorkWindow Vertical $ vBox $ ioWidget dis

-- |Creates a widget for I/O
ioWidget :: [(Text,Text)] -> [Widget RName]
ioWidget = reverse . foldl' (\acc (i,o) ->
                               if Text.null o
                               then (Brick.Widgets.Core.showCursor CName
                                     (Location (3,0))
                                      (txt (Text.concat
                                            [" > ",i]))):acc
                               else (Brick.Widgets.Core.showCursor CName
                                     (Location (3,0))
                                      (txt (Text.concat
                                            [" > ",i,"\n",o]))):acc) []

-- |Listen for and handle events from the endpoint
listenAtEnd :: EndPoint -> IO ()
listenAtEnd end = receive end >>=
  (\event -> case event of
      Received _ info -> forkIO (print info) >>
        listenAtEnd end
      ConnectionClosed _ -> putStrLn "Connection Closed (Client)"
      EndPointClosed -> putStrLn "EndPoint Closed (Client)"
      _ -> listenAtEnd end)
