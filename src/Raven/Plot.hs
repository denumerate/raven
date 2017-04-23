{-# LANGUAGE OverloadedStrings #-}
module Raven.Plot
  ( buildPlot
  ) where

import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Cairo

import Data.Time.Clock
import System.Directory

-- |Builds the passed graph and stores it in the graph folder (name is timestamp)
buildPlot :: (Default r,ToRenderable r) => EC r () -> IO String
buildPlot graph = getHomeDirectory >>=
  (\home -> withCurrentDirectory (home ++ "/.raven/plots")
    (getCurrentTime >>=
     (\time -> let name = buildPlotName time
       in toFile def name graph >>
          return name)))

-- |Creates the file name using the utctime
buildPlotName :: UTCTime -> String
buildPlotName time =
  head (tail (words (show time))) ++ ".png"
