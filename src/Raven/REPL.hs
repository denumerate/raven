module Raven.REPL ( interp
                  , interpIO
                  , initREPL
                  , runIO
                  ) where

import Language.Haskell.Interpreter
import Data.List (intercalate)

-- |Interpret a string, return the result
interp :: Interpreter () -> String -> IO String
interp interpS value = runInterpreter (interpS >> eval value) >>=
  (\out -> case out of
      Left err -> return (errorString err)
      Right out' -> return out')

-- |Interpret an io, Nothing if it worked, String contains errors
interpIO :: Interpreter () -> String -> IO (Maybe String)
interpIO interpS value = runInterpreter (interpS >> runIO value) >>=
  (\out -> case out of
      Left err -> return $ Just $ errorString err
      _ -> return Nothing)

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
  , ("Raven.Plot", Nothing)
  , ("Graphics.Rendering.Chart.Easy", Nothing)
  ]

-- |io interpreter wrapper
runIO :: (MonadInterpreter m) => String -> m ()
runIO code = interpret code (as :: IO ()) >>= liftIO
