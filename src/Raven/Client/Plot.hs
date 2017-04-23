{-# LANGUAGE OverloadedStrings #-}
module Raven.Plot
  ( buildPlot
  ) where

import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Cairo

import Data.Time.Clock
import System.Directory

import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.List (foldl')

import Language.Haskell.Interpreter

-- |parses and plots a plot command
plotGraph :: ByteString -> IO ()
plotGraph bstring = let vals = B.split ' ' (B.drop 6 bstring)
  in case getType vals of
       Just "line" -> print "line"
       _ -> return ()

-- |finds the type info from the plot command
getType :: [ByteString] -> Maybe ByteString
getType bstrings = foldl' (check "type=") Nothing bstrings

-- |Builds the passed graph and stores it in the graph folder (name is timestamp)
buildPlot :: (Default r,ToRenderable r) => EC r () -> IO ()
buildPlot graph = getHomeDirectory >>=
  (\home -> withCurrentDirectory (home ++ "/.raven/client/plots")
    (getCurrentTime >>=
     (\time -> toFile def ("" ++ show time ++ ".png") graph)))

-- |build a line graph with a function and a set of points
linePlot :: (Float -> Float) -> [Float] -> IO ()
linePlot f ps = buildPlot $ plot $ line "line" [map (\p -> (p,f p)) ps]

-- |io interpreter wrapper
runIO :: (MonadInterpreter m) => String -> m ()
runIO code = interpret code (as :: IO ()) >>= liftIO

-- |generic check function for parsing the plot command
check :: ByteString -> Maybe ByteString -> ByteString -> Maybe ByteString
check test Nothing bstring = if B.isPrefixOf test bstring
  then Just $ B.drop (B.length test) bstring else Nothing
