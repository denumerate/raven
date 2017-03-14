module Raven.Client.GUI
  ( guiMain
  )where

import Network.Transport

import Control.Monad
import Control.Monad.IO.Class
import Data.IORef
import Graphics.UI.Gtk hiding (Action, backspace)
import System.Glib.UTFString

-- |Builds and runs ui
guiMain :: Connection -> EndPoint -> EndPointAddress -> IO ()
guiMain conn end server = void initGUI >>
  windowNew >>=
  (\w ->
     set w windowSettings >>
     windowFullscreen w >>
     widgetShowAll w) >>
  mainGUI

-- |window settings
windowSettings :: (WindowClass o) => [AttrOp o]
windowSettings =
  [ windowTitle := "raven"
  ]
