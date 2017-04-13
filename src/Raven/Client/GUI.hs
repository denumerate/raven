module Raven.Client.GUI
  ( guiMain
  )where

import Network.Transport
import Control.Concurrent

import Control.Monad.IO.Class
import Control.Monad
import Graphics.UI.Gtk hiding (Action, backspace)

import Raven.Client.Connection

-- |Builds and runs ui
guiMain :: Connection -> EndPoint -> EndPointAddress -> IO ()
guiMain conn end server = void initGUI >>
  windowNew >>=
  (\w ->
     windowSettings conn w >>
     buildConnInfo end server >>=
     (\connL ->
        vBoxNew False 2 >>=
        (\vbox ->
           boxPackStart vbox connL PackNatural 2 >>
           buildWork conn >>=
           (\ws -> boxPackEnd vbox ws PackGrow 2) >>
           containerAdd w vbox)) >>
     widgetShowAll w) >>
  mainGUI

-- |window settings
windowSettings :: Connection -> Window -> IO ()
windowSettings conn w =
  set w [ windowTitle := "raven"
        ]>>
  windowFullscreen w >>
  on w deleteEvent (liftIO (close conn) >>
                    liftIO mainQuit >>
                    return False) >>
  return ()

-- |Creates label that displays connection info and starts up the listening process
buildConnInfo :: EndPoint -> EndPointAddress -> IO TextView
buildConnInfo end server =
  textBufferNew Nothing >>=
  (\tbuf ->
     forkIO (listenAtEnd end tbuf) >>
     textBufferSetText tbuf ("Connection Established: " ++ (show server)) >>
     textViewNewWithBuffer tbuf >>=
     (\tview ->
        set tview [ textViewCursorVisible := False
                  , textViewEditable := False
                  ] >>
        return tview))

-- |Creates the widget that allows for work
buildWork :: Connection -> IO HBox
buildWork conn =
  hBoxNew False 2 >>=
  (\vbox ->
     vScrollbarNewDefaults >>=
     (\s -> boxPackEnd vbox s PackNatural 2 >>
            inOuts conn >>=
            (\ios -> boxPackStart vbox ios PackGrow 2)) >>
     return vbox)

-- |creates input output areas
inOuts :: Connection -> IO HBox
inOuts conn =
  hBoxNew False 2 >>=
  (\hbox -> inOut >>=
    (\io -> boxPackStart hbox io PackGrow 2 >>
            return hbox))
  where
    inOut = hBoxNew False 2 >>=
      (\hbox -> textViewNew >>=
        (\i -> boxPackStart hbox i PackGrow 2 >>
               textViewSetCursorVisible i True >>
               textViewSetWrapMode i WrapWordChar >>
               textViewGetBuffer i >>=
               (\buf -> newMVar 0 >>=
                        on i keyPressEvent . next buf) >>
               return hbox))
    next buf lastKey = eventKeyVal >>=
      (\key -> liftIO (takeMVar lastKey) >>=
        (\lastKey' -> case (key,lastKey') of
            (65505,65293) -> liftIO (textBufferGetStartIter buf) >>=
                             (\srt ->
                                liftIO (textBufferGetEndIter buf) >>=
                                (\end ->
                                   liftIO (textBufferGetByteString buf
                                           srt end False))) >>=
                             liftIO . sendReq conn 0 >>
                             liftIO (putMVar lastKey 0)
            (newKey,_) -> liftIO (putMVar lastKey newKey))) >>
      return False
