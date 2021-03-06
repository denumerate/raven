{-# LANGUAGE OverloadedStrings #-}
module Raven.Plot
  ( buildPlot
  ) where

import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Cairo

import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Lazy as LB

import Data.Time.Clock
import System.Directory
import Codec.Picture

-- |Builds the passed graph and stores it in the graph folder (name is timestamp)
buildPlot :: (Default r,ToRenderable r) => EC r () ->
  IO (Either String ByteString)
buildPlot graph = getHomeDirectory >>=
  (\home -> withCurrentDirectory (home ++ "/.raven/plots")
    (getCurrentTime >>=
     (\time -> let name = buildPlotName time
       in toFile def name graph >>
          readImage name >>=
          (\img -> case img of
              Left str -> return $ Left str
              Right img' -> case encodeDynamicBitmap img' of
                Left str -> return $ Left str
                Right bstring ->
                  removeFile name >>
                  return (Right (LB.toStrict bstring))))))

-- |Creates the file name using the utctime
buildPlotName :: UTCTime -> String
buildPlotName time =
  map (\c -> if c == ':' then '_' else c) $
  head (tail (words (show time))) ++ ".png"
