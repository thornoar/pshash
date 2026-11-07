module Main where

import System.Environment (getArgs)

import Error
import Actions
import Parsing

-- ┌────────────────┐
-- │ USER INTERFACE │
-- └────────────────┘

main :: IO ()
main = do
  rawArgs <- getArgs
  let parsedArgs = addTrace "Parsing command-line options:" $ parseArgs (False, False, False) rawArgs
  toIO rawArgs $ handleWith' performAction (setEchoesAndPrompts <$> parsedArgs)
