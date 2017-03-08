{-# LANGUAGE OverloadedStrings #-}
module Raven.Client.Handler
  ( handler
  )where

import Network.Transport
import Control.Concurrent

import Brick.Main
import Brick.Types
import Graphics.Vty

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.ByteString.Char8 as B
import Data.ByteString.Char8 (ByteString)
import Text.Read
import Data.Vector (Vector)
import qualified Data.Vector as V

-- |Handle events
handler :: Connection -> (Maybe ByteString,Text,Vector (Text,Text,Int),Int) ->
  BrickEvent n [Text] ->
  EventM n (Next (Maybe ByteString, Text,Vector (Text,Text,Int),Int))
handler conn s (VtyEvent (EvKey k mods)) = handleKey conn s k mods
handler _ s (AppEvent e) = handleApp s e
handler _ s _ = continue s

-- |Handle key events
handleKey :: Connection -> (Maybe ByteString,Text,Vector (Text,Text,Int),Int) ->
  Key -> [Modifier] ->
  EventM n (Next (Maybe ByteString,Text,Vector (Text,Text,Int),Int))
handleKey _ (tok,c,ios,hs) (KChar chr) [] = let (i,o,cur) = V.last ios in
  continue (tok,c,V.snoc (V.init ios) (Text.snoc i chr,o,cur+1),hs)
handleKey _ s@(tok,c,ios,hs) (KDel) [] = let (i,o,cur) = V.last ios in
  if Text.null i
  then continue s
  else continue (tok,c,V.snoc (V.init ios) (delete (cur-4) i,o,cur-1),hs)
handleKey c s (KBS) [] = handleKey c s KDel []
handleKey conn s (KEnter) [] = handleLine conn s
handleKey _ s@(tok,c,ios,hs) (KLeft) [] =
  let (i,o,cur) = V.last ios in
    if cur > 3
    then continue (tok,c,V.snoc (V.init ios) (i,o,cur-1),hs)
    else continue s
handleKey _ s@(tok,c,ios,hs) (KRight) [] =
  let (i,o,cur) = V.last ios in
    if cur < 3 + Text.length i
    then continue (tok,c,V.snoc (V.init ios) (i,o,cur+1),hs)
    else continue s
handleKey _ s@(tok,c,ios,hs) (KUp) [] =
  let (_,o,_) = V.last ios
      hs' = hs - 1
    in if hs > 0
       then let (i',_,_) = ios V.! hs' in
              continue (tok,c,V.snoc (V.init ios)
                         (i',o,3 + Text.length i'),hs')
       else continue s
handleKey _ s@(tok,c,ios,hs) (KDown) [] =
  let (_,o,_) = V.last ios
      hs' = hs + 1
    in if hs < V.length ios - 2
       then let (i',_,_) = ios V.! hs' in
              continue (tok,c,V.snoc (V.init ios)
                         (i',o,3 + Text.length i'),hs')
       else if hs == V.length ios - 2
            then continue (tok,c,V.snoc (V.init ios)
                            ("",o,3),hs')
            else continue s
handleKey _ s _ _ = continue s

-- |Delete character at index i
delete :: Int -> Text -> Text
delete i t = Text.concat
  [ Text.take i t
  , Text.drop (i+1) t
  ]

handleLine :: Connection -> (Maybe ByteString,Text,Vector (Text,Text,Int),Int) ->
  EventM n (Next (Maybe ByteString,Text,Vector (Text,Text,Int),Int))
handleLine conn s@(tok,c,ios,_) = let (info,_,_) = V.last ios in
  if Text.null info
  then continue (tok,c,V.snoc ios ("","",3),V.length ios)
  else if Text.head info == ':' then handleCommand conn s
       else case tok of
              Just tok' ->
                suspendAndResume
                (forkIO (send conn
                         [tok'
                         , " "
                         , B.pack $ show $ length ios - 1
                         , " "
                         , B.pack $ Text.unpack info --error possible
                         ] >>
                         return ()) >>
                 return (tok,c,V.snoc ios ("","",3),V.length ios))
              _ -> continue (tok,c,V.snoc ios ("","",3),V.length ios)

handleCommand :: Connection -> (Maybe ByteString,Text,Vector (Text,Text,Int),Int) ->
  EventM n (Next (Maybe ByteString,Text,Vector (Text,Text,Int),Int))
handleCommand conn s@(tok,c,ios,_) = let (cmd,_,_) = V.last ios
                                         cmd' = head (Text.words cmd) in
  case cmd' of
    ":quit" -> halt s
    ":logon" -> suspendAndResume
                (forkIO (send conn
                         [ "logon "
                         , B.pack $ show $ length ios - 1
                         , " "
                         , B.pack $ Text.unpack cmd --error possible
                         ] >>
                         return ()) >>
                 return (tok,c,V.snoc ios ("","",3),V.length ios))
    _ -> case tok of
              Just tok' ->
                suspendAndResume
                (forkIO (send conn
                         [tok'
                         , " "
                         , B.pack $ show $ length ios - 1
                         , " "
                         , B.pack $ Text.unpack cmd --error possible
                         ] >>
                         return ()) >>
                 return (tok,c,V.snoc ios ("","",3),V.length ios))
              _ -> continue (tok,c,V.snoc ios ("","",3),V.length ios)

-- |Handle app events
handleApp :: (Maybe ByteString,Text,Vector (Text,Text,Int),Int) -> [Text] ->
  EventM n (Next (Maybe ByteString,Text,Vector (Text,Text,Int),Int))
handleApp (tok,con,ios,hs) [msg] = if Text.isPrefixOf "Connection Error:" msg
  then continue (tok,msg,ios,hs)
  else continue (Just $ B.pack $ Text.unpack msg,con,ios,hs)
handleApp s@(tok,c,ios,hs) [n,out] = case (readMaybe (Text.unpack n) :: Maybe Int) of
  Just n' -> let (i,_,cur) = ios V.! n' in
    continue (tok,c,ios V.// [(n',(i,out,cur))],hs)
  _ -> continue s --this is an error and should be handled
