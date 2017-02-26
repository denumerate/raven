module Raven.Client.Handler
  ( handler
  )where

import Brick.Main
import Brick.Types
import Graphics.Vty

import Data.Text (Text)
import qualified Data.Text as Text

-- |Handle events
handler :: (Text,[(Text,Text)]) -> BrickEvent n e ->
  EventM n (Next (Text,[(Text,Text)]))
handler s (VtyEvent (EvKey k mods)) = handleKey s k mods
handler s _ = continue s

handleKey :: (Text,[(Text,Text)]) -> Key -> [Modifier] ->
  EventM n (Next (Text,[(Text,Text)]))
handleKey (c,ios) (KChar chr) [] = let (i,o) = last ios in
  continue (c,init ios ++ [(Text.snoc i chr,o)])
handleKey s _ _ = continue s
