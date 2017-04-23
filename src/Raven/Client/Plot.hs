{-# LANGUAGE OverloadedStrings #-}
module Raven.Client.Plot
  (
  ) where

import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Cairo

import Data.Time.Clock
import System.Directory

import Data.List (foldl')
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B

-- |graph type
--data GraphType =

-- |parses and plots a plot command
plotGraph :: ByteString -> IO ()
plotGraph bstring = let vals = B.split ' ' (B.drop 6 bstring)
  in case getType vals of
       Just "line" -> 
       _ -> return ()

-- |finds the type info from the plot command
getType :: [ByteString] -> Maybe ByteString
getType bstrings =
  foldl' check Nothing bstrings
  where
    check Nothing val =
      if B.isPrefixOf "type=" val then Just $ B.drop 5 val
      else Nothing
    check acc _ = acc
-- |Builds the passed graph and stores it in the graph folder (name is timestamp)
buildPlot :: (Default r,ToRenderable r) => EC r () -> IO ()
buildPlot graph = getHomeDirectory >>=
  (\home -> withCurrentDirectory (home ++ "/.raven/client/plots")
    (getCurrentTime >>=
     (\time -> toFile def ("" ++ show time ++ ".png") graph)))

-- |build a line graph with a function and a set of points
linePlot :: (Float -> Float) -> [Float] -> IO ()
linePlot f ps = buildPlot $ plot $ line "line" [map (\p -> (p,f p)) ps]
