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
import Data.List

import Raven.Client.Handler

data RName = WorkWindow
           | CName
  deriving (Eq,Ord,Show)

buffersize = 25

-- |Builds and runs terminal ui
-- State is: Connection info,Vector of lines,history int
guiMain :: Connection -> EndPoint -> EndPointAddress -> IO ()
guiMain conn end server =
  let app =
        (App
          { appDraw = myDraw
          , appChooseCursor = handleCursor
          , appHandleEvent = handler conn
          , appStartEvent = return
          , appAttrMap = const $ attrMap Graphics.Vty.defAttr []
          } :: App (Text,Vector (Text,Text,Int),Int) [Text] RName)
  in newBChan buffersize >>=
     (\chan ->
         forkIO (listenAtEnd chan end) >>
         customMain (mkVty defaultConfig) (Just chan) app
         ((Text.pack . show) server,V.fromList [("","",3)],0)) >>
     return ()

-- |All widgets
myDraw :: (Text,Vector (Text,Text,Int),Int) -> [Widget RName]
myDraw (c,ios,_) = [ vBox [ hBorder
                        , connStatus c
                        , hBorder
                        , workWidget ios
                        ]
                 ]

-- |Connection Status Widget
connStatus :: Text -> Widget n
connStatus s = if Text.isPrefixOf "Connection Error:" s
               then withBorderStyle unicode $
                    padAll 1 $ txt s
               else withBorderStyle unicode $
                    padAll 1 $ txt $ Text.concat
                    ["Connection: ",s]

-- |Creates a work Widget
workWidget :: Vector (Text,Text,Int) -> Widget RName
workWidget dis = viewport WorkWindow Vertical $ vBox $ ioWidget dis

-- |Creates a widget for I/O
ioWidget :: Vector (Text,Text,Int) -> [Widget RName]
ioWidget = reverse . V.foldl' (\acc (i,o,c) ->
                               if Text.null o
                               then (Brick.Widgets.Core.showCursor CName
                                     (Location (c,0))
                                      (txt (Text.concat
                                            [" > ",i]))):acc
                               else (Brick.Widgets.Core.showCursor CName
                                     (Location (c,0))
                                      (txt (Text.concat
                                            [" > ",i,"\n ",o]))):acc) []

-- |Handles the cursor
handleCursor :: (Text,Vector (Text,Text,Int),Int) -> [CursorLocation n] ->
  Maybe (CursorLocation n)
handleCursor _ = foldl' choose Nothing
  where
    choose Nothing val = Just val
    choose (Just acc@(CursorLocation (Location (_,accY)) _))
      val@(CursorLocation (Location (_,valY)) _) =
      if accY > valY then Just acc else Just val

-- |Listen for and handle events from the endpoint
listenAtEnd :: BChan [Text] -> EndPoint -> IO ()
listenAtEnd chan end = receive end >>=
  (\event -> case event of
      Received _ [info] ->
        case B.words info of
          tok@[_] -> forkIO (writeBChan chan
                           (map (Text.pack . B.unpack)
                            tok)) >>
             listenAtEnd chan end
          wds -> forkIO (writeBChan chan
                          (map (Text.pack . B.unpack)
                            [head wds,B.unwords (tail wds)])) >>
             listenAtEnd chan end
      ConnectionClosed _ -> writeBChan chan ["Connection Error: Connection Closed"]
      EndPointClosed -> writeBChan chan ["Connection Error: Endpoint Closed"]
      _ -> listenAtEnd chan end)
