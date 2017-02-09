module Raven.REPL ( interp
                  ) where

import Language.Haskell.Interpreter
import System.IO
import Data.List (intercalate)

-- |Interpret a string, return the result
interp :: Handle -> IO ()
interp handle =
  hGetLine handle >>=
  (\line ->
     runInterpreter (initREPL >> eval line >>=
                     liftIO . hPutStrLn handle)) >>
  interp handle

-- | prints errors
errorString :: InterpreterError -> String
errorString (WontCompile es) = intercalate "\n" (header : map unbox es)
  where
    header = "ERROR: Won't compile:"
    unbox (GhcError e) = e
errorString e = show e

-- |initializes the repl
initREPL :: MonadInterpreter m => m ()
initREPL = setImportsQ
  [ ("Prelude", Nothing)]
