module Raven.Client.GUI
  ( guiMain
  )where

import Network.Transport
import Control.Concurrent

import Control.Monad.IO.Class
import Control.Monad
import Graphics.UI.Gtk hiding (Action, backspace)

import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B

import Raven.Client.Connection

-- |Builds and runs ui
guiMain :: Connection -> EndPoint -> EndPointAddress -> IO ()
guiMain conn end server = void initGUI >>
  windowNew >>=
  (\w ->
     windowSettings conn w >>
     textBufferNew Nothing >>=
     (\buf -> buildConnInfo end server buf >>=
       (\connL ->
          vBoxNew False 2 >>=
         (\vbox ->
            boxPackStart vbox connL PackNatural 2 >>
            buildWork conn buf >>=
            (\ws -> boxPackEnd vbox ws PackGrow 2) >>
            containerAdd w vbox))) >>
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
buildConnInfo :: EndPoint -> EndPointAddress -> TextBuffer -> IO TextView
buildConnInfo end server buf =
  textBufferNew Nothing >>=
  (\tbuf ->
     forkIO (listenAtEnd end tbuf buf) >>
     textBufferSetText tbuf ("Connection Established: " ++ (show server)) >>
     textViewNewWithBuffer tbuf >>=
     (\tview ->
        set tview [ textViewCursorVisible := False
                  , textViewEditable := False
                  ] >>
        return tview))

-- |Creates the widget that allows for work
buildWork :: Connection -> TextBuffer -> IO HBox
buildWork conn buf =
  hBoxNew False 2 >>=
  (\vbox ->
     vScrollbarNewDefaults >>=
     (\s -> boxPackEnd vbox s PackNatural 2 >>
            inOuts conn buf >>=
            (\ios -> boxPackStart vbox ios PackGrow 2)) >>
     return vbox)

-- |creates input output areas
inOuts :: Connection -> TextBuffer -> IO TextView
inOuts conn buf = textViewNewWithBuffer buf >>=
  (\i -> textViewSetCursorVisible i True >>
         textViewSetWrapMode i WrapWordChar >>
         newMVar 0 >>=
         on i keyPressEvent . next >>
         return i)
  where
    next lastKey = eventKeyVal >>=
      (\key -> liftIO (takeMVar lastKey) >>=
        (\lastKey' -> case (key,lastKey') of
            (65293,65505) -> handleShiftRet >>
                             liftIO (putMVar lastKey 0)
            (65293,65506) -> handleShiftRet >>
                             liftIO (putMVar lastKey 0)
            (newKey,_) -> liftIO (putMVar lastKey newKey))) >>
      return False
    handleShiftRet = liftIO (parseBuf buf) >>=
      liftIO . sendReq conn 0

-- |Get the last entry in the buffer
parseBuf :: TextBuffer -> IO ByteString
parseBuf buf = textBufferGetStartIter buf >>=
  (\start -> textBufferGetEndIter buf >>=
    (\end -> textBufferGetByteString buf start end False)) >>=
  return . last . B.split '\n'
