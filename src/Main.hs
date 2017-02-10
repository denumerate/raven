module Main where

import Raven.Server

main :: IO ()
main = initServer "127.0.0.1" "4444" >>= print
