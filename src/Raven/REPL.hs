module Raven.REPL ( interp
                  ) where

import Language.Haskell.Interpreter
import Data.List (intercalate)

-- |Interpret a string, return the result
interp :: String -> IO String
interp value = runInterpreter (initREPL >> eval value) >>=
  (\out -> case out of
      Left err -> return (errorString err)
      Right out' -> return out')

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
