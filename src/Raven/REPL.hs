module Raven.REPL ( runREPL
                  ) where

import Language.Haskell.Interpreter
import System.IO
import Data.List (intercalate)

-- |Runs the repl, top level
runREPL :: IO ()
runREPL = run

-- |runs the repl, internal
run :: IO ()
run = inputREPL >>=
  (\input -> runInterpreter $ initREPL >> evalREPL input) >>=
  (\out -> case out of
      Left err -> putStrLn (errorString err) >>
        run
      _ -> run)

-- |Handles input, creates interface
inputREPL :: IO String
inputREPL = putStr "Raven> " >>
  hFlush stdout >>
  getLine

-- | uses eval to evaluate and print output
evalREPL :: MonadInterpreter m => String -> m ()
evalREPL input = eval input >>=
  (liftIO . putStrLn)

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
