module Raven.Client.GUI
  ( guiMain
  )where

import Network.Transport
import Control.Concurrent
import System.Directory

import Control.Monad.IO.Class
import Control.Monad
import Graphics.UI.Gtk hiding (Action, backspace)

import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.Vector (Vector)
import qualified Data.Vector as V

import Raven.Client.Connection

-- |WorkVector holds all the io data for the work area
type WorkVector = MVar (Vector TextBuffer)

-- |Builds and runs ui
guiMain :: Connection -> EndPoint -> EndPointAddress -> IO ()
guiMain conn end server = buildFilePaths >>
  void initGUI >>
  windowNew >>=
  (\w ->
     windowSettings conn w >>
     textBufferNew Nothing >>=
     (\buf -> buildConnInfo end server >>=
       (\connL ->
          vBoxNew False 2 >>=
         (\vbox ->
            boxPackStart vbox connL PackNatural 2 >>
            newMVar (V.singleton buf) >>=
            (\wvec ->
               buildWork conn wvec >>=
               (\ws -> boxPackEnd vbox ws PackGrow 2) >>
               textViewGetBuffer connL >>=
               (\cbuf ->
                  forkIO (listenAtEnd end cbuf wvec))) >>
            containerAdd w vbox))) >>
     widgetShowAll w) >>
  mainGUI

-- |Ensures that the correct filepaths exist
buildFilePaths :: IO ()
buildFilePaths = getHomeDirectory >>=
  setCurrentDirectory >>
  createDirectoryIfMissing True ".raven/client/plots"

-- |window settings
windowSettings :: Connection -> Window -> IO ()
windowSettings conn w =
  set w [ windowTitle := "raven"
        ]>>
  windowFullscreen w >>
  on w deleteEvent (liftIO cleanFiles >>
                    liftIO (close conn) >>
                    liftIO mainQuit >>
                    return False) >>
  return ()

-- |cleans up extra files
cleanFiles :: IO ()
cleanFiles = removeDirectoryRecursive ".raven/client/plots"

-- |Creates label that displays connection info and starts up the listening process
buildConnInfo :: EndPoint -> EndPointAddress -> IO TextView
buildConnInfo end server =
  textBufferNew Nothing >>=
  (\tbuf ->
     textBufferSetText tbuf ("Connection Established: " ++ show server) >>
     textViewNewWithBuffer tbuf >>=
     (\tview ->
        set tview [ textViewCursorVisible := False
                  , textViewEditable := False
                  ] >>
        return tview))

-- |Creates the widget that allows for work
buildWork :: Connection -> WorkVector -> IO HBox
buildWork conn wVector =
  hBoxNew False 2 >>=
  (\vbox ->
     vScrollbarNewDefaults >>=
     (\s -> boxPackEnd vbox s PackNatural 2 >>
            inOuts conn wVector >>=
            (\ios -> boxPackStart vbox ios PackGrow 2)) >>
     return vbox)

-- |creates input output areas
inOuts :: Connection -> WorkVector -> IO VBox
inOuts conn wVector = vBoxNew False 2 >>=
  (\vbox ->
     readMVar wVector >>=
     V.mapM_ (build vbox) >>
  return vbox)
  where
    build vbox buf =
      textViewNewWithBuffer buf >>=
      (\i ->
          textViewSetCursorVisible i True >>
          textViewSetWrapMode i WrapWordChar >>
          newMVar 0 >>=
          on i keyPressEvent . next vbox buf >>
          boxPackStart vbox i PackGrow 2 >>
          textViewSetCursorVisible i True)
    next vbox buf lastKey = eventKeyVal >>=
      (\key -> liftIO (takeMVar lastKey) >>=
        (\lastKey' -> case (key,lastKey') of
            (65293,65505) -> handleShiftRet vbox buf >>
                             liftIO (putMVar lastKey 0)
            (65293,65506) -> handleShiftRet vbox buf >>
                             liftIO (putMVar lastKey 0)
            (newKey,_) -> liftIO (putMVar lastKey newKey))) >>
      return False
    handleShiftRet vbox buf = liftIO (takeMVar wVector) >>=
                         (\wVector' ->
                            liftIO (textBufferNew Nothing) >>=
                            (\nbuf ->
                               (liftIO . putMVar wVector . V.snoc wVector') nbuf >>
                               liftIO (build vbox nbuf)) >>
                            liftIO (widgetShowAll vbox) >>
                            return (length wVector' - 1)) >>=
                         (\n -> liftIO (parseBuf buf) >>=
                                liftIO . sendReq conn n)

-- |Get the last entry in the buffer
parseBuf :: TextBuffer -> IO ByteString
parseBuf buf = fmap (\bstring ->
                       if B.null bstring then B.empty
                       else (last . B.split '\n') bstring) $
  textBufferGetStartIter buf >>=
  (\start -> textBufferGetEndIter buf >>=
    (\end -> textBufferGetByteString buf start end False))
