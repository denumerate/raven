{-# LANGUAGE OverloadedStrings #-}
module Raven.Client.GUI
  ( guiMain
  )where

import Network.Transport

import Control.Monad
import Control.Monad.IO.Class
import Data.IORef
import Graphics.UI.Gtk hiding (Action, backspace)

-- |Builds and runs terminal ui
-- State is: Token,Connection info,Vector of lines,history int
guiMain :: Connection -> EndPoint -> EndPointAddress -> IO ()
guiMain conn end server = do
  void initGUI
  window <- windowNew
  widgetShowAll window
  mainGUI
