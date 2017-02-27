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
import Text.Read
import Data.Vector (Vector)
import qualified Data.Vector as V

-- |Handle events
handler :: Connection -> (Text,Vector (Text,Text)) -> BrickEvent n [Text] ->
  EventM n (Next (Text,Vector (Text,Text)))
handler conn s (VtyEvent (EvKey k mods)) = handleKey conn s k mods
handler _ s (AppEvent e) = handleApp s e
handler _ s _ = continue s

-- |Handle key events
handleKey :: Connection -> (Text,Vector (Text,Text)) -> Key -> [Modifier] ->
  EventM n (Next (Text,Vector (Text,Text)))
handleKey _ (c,ios) (KChar chr) [] = let (i,o) = V.last ios in
  continue (c,V.snoc (V.init ios) (Text.snoc i chr,o))
handleKey _ s@(c,ios) (KDel) [] = let (i,o) = V.last ios in
  if Text.null i
  then continue s
  else continue (c,V.snoc (V.init ios) (Text.init i,o))
handleKey c s (KBS) [] = handleKey c s KDel []
handleKey conn s (KEnter) [] = handleLine conn s
handleKey _ s _ _ = continue s

handleLine :: Connection -> (Text,Vector (Text,Text)) ->
  EventM n (Next (Text,Vector (Text,Text)))
handleLine conn s@(c,ios) = let info = fst $ V.last ios in
  if Text.null info
  then continue (c,V.snoc ios ("",""))
  else if Text.head info == ':'
       then handleCommand conn s
       else suspendAndResume
            (forkIO (send conn
                     [ B.pack $ show $ length ios - 1
                     , " "
                     , B.pack $ Text.unpack info --error possible
                     ] >>
                     return ()) >>
             return (c,V.snoc ios ("","")))

handleCommand ::  Connection -> (Text,Vector (Text,Text)) ->
  EventM n (Next (Text,Vector (Text,Text)))
handleCommand conn s@(c,ios) = let cmd = fst $ V.last ios in
  case cmd of
    ":quit" -> halt s
    ":kill" -> suspendAndResume
               (forkIO (send conn
                         [":kill"] >>
                         return ()) >>
                return (c,V.snoc ios ("","")))
    _ -> continue (c,V.snoc ios ("",""))

-- |Handle app events
handleApp :: (Text,Vector (Text,Text)) -> [Text] ->
  EventM n (Next (Text,Vector (Text,Text)))
handleApp (_,ios) [conErr] = continue (conErr,ios)
handleApp s@(c,ios) [n,out] = case (readMaybe (Text.unpack n) :: Maybe Int) of
  Just n' -> let (i,_) = ios V.! n' in
    continue (c,ios V.// [(n',(i,out))])
  _ -> continue s --this is an error and should be handled
