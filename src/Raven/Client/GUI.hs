module Raven.Client.GUI
  ( guiMain
  )where

import Network.Transport

import Control.Monad.IO.Class
import Control.Monad
import Graphics.UI.Gtk hiding (Action, backspace)

-- |Builds and runs ui
guiMain :: Connection -> EndPoint -> EndPointAddress -> IO ()
guiMain conn end server = void initGUI >>
  windowNew >>=
  (\w ->
     windowSettings w >>
     buildConnInfo server >>=
     containerAdd w >>
     widgetShowAll w) >>
  mainGUI

-- |window settings
windowSettings :: Window -> IO ()
windowSettings w =
  set w [ windowTitle := "raven"
        ]>>
  windowFullscreen w >>
  on w deleteEvent (liftIO mainQuit >> return False) >>
  return ()

buildConnInfo :: EndPointAddress -> IO TextView
buildConnInfo server =
  textBufferNew Nothing >>=
  (\tbuf ->
     textBufferSetText tbuf ("Connection Established: " ++ (show server)) >>
     textViewNewWithBuffer tbuf >>=
     (\tview ->
        set tview [ textViewCursorVisible := False
                  , textViewEditable := False
                  ] >>
        return tview))
