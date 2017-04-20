module Raven.REPL ( interp
                  , initREPL
                  ) where

import Language.Haskell.Interpreter
import Data.List (intercalate)

-- |Interpret a string, return the result
interp :: Interpreter () -> String -> IO String
interp interpS value = runInterpreter (interpS >> eval value) >>=
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
initREPL :: Interpreter ()
initREPL = setImportsQ
  [ ("Prelude", Nothing)
  , ("Raven.Data.Stat", Nothing)
  , ("Raven.Data.Prob", Nothing)
  , ("Raven.Data.Prob.DistributionFuncs", Nothing)
  , ("Raven.Data.Entry", Nothing)
  , ("Raven.Data.BasicEntry", Nothing)
  , ("Raven.Data.BasicUnboundEntry", Nothing)
  , ("Raven.Data.Table", Nothing)
  ]
