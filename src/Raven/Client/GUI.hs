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
import qualified Data.ByteString.Char8 as B
import Data.Vector (Vector)
import qualified Data.Vector as V

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
                           , appHandleEvent = handler conn
                           , appStartEvent = return
                           , appAttrMap = const $ attrMap Graphics.Vty.defAttr []
                           } :: App (Text,Vector (Text,Text)) [Text] RName)
                   in newBChan buffersize >>=
                      (\chan ->
                         forkIO (listenAtEnd chan end) >>
                         customMain (mkVty defaultConfig) (Just chan) app
                         ((Text.pack . show . address) end,V.fromList [("","")])) >>
                      return ()

-- |All widgets
myDraw :: (Text,Vector (Text,Text)) -> [Widget RName]
myDraw s = [ vBox [ hBorder
                  , connStatus s
                  , hBorder
                  , workWidget s
                  ]
           ]

-- |Connection Status Widget
connStatus :: (Text,Vector (Text,Text)) -> Widget n
connStatus (s,_) = if Text.isPrefixOf "Connection Error:" s
               then withBorderStyle unicode $
                    padAll 1 $ txt s
               else withBorderStyle unicode $
                    padAll 1 $ txt $ Text.concat
                    ["Connection: ",s]

-- |Creates a work Widget
workWidget :: (Text,Vector (Text,Text)) -> Widget RName
workWidget (_,dis) = viewport WorkWindow Vertical $ vBox $ ioWidget dis

-- |Creates a widget for I/O
ioWidget :: Vector (Text,Text) -> [Widget RName]
ioWidget = reverse . V.foldl' (\acc (i,o) ->
                               if False
                               then (Brick.Widgets.Core.showCursor CName
                                     (Location (3,0))
                                      (txt (Text.concat
                                            [" > ",i]))):acc
                               else (Brick.Widgets.Core.showCursor CName
                                     (Location (3,0))
                                      (txt (Text.concat
                                            [" > ",i,"\n ",o]))):acc) []

-- |Listen for and handle events from the endpoint
listenAtEnd :: BChan [Text] -> EndPoint -> IO ()
listenAtEnd chan end = receive end >>=
  (\event -> case event of
      Received _ [info] -> let wds = B.words info in
                             forkIO (writeBChan chan
                                 (map (Text.pack . B.unpack)
                                 [head wds,B.unwords (tail wds)])) >>
        listenAtEnd chan end
      ConnectionClosed _ -> writeBChan chan ["Connection Error: Connection Closed"]
      EndPointClosed -> writeBChan chan ["Connection Error: Endpoint Closed"]
      _ -> listenAtEnd chan end)
