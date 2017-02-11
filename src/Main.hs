module Main where

import Raven.Server
import Raven.Client

main :: IO ()
main = initServer "127.0.0.1" "4444" >>=
  (\ser -> case ser of
      Just ser' -> initClient "127.0.0.1" "1234" ser'
      _ -> return Nothing) >>
  return ()
