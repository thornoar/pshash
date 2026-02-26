module Main where

import System.IO (stderr, hPutStrLn, hSetBuffering, stdin, BufferMode (NoBuffering), hSetEcho)
import Data.Map (Map, member, (!))
import System.Environment (getArgs)
import System.Info (os)
import System.Exit (exitWith, ExitCode (ExitFailure))

import Algorithm
import Error
import Encryption
import Actions
import Parsing

-- ┌────────────────┐
-- │ USER INTERFACE │
-- └────────────────┘

passKeysToAction ::
  Map OptionName String ->
  (String -> String -> String -> IO (Result ())) ->
  IO (Result ())
passKeysToAction args act = do
  first <- getKeyStr args FIRST E1 P1
  second <- getKeyStr args SECOND E2 P2
  third <- getKeyStr args THIRD E3 P3
  act first second third

performAction :: Map OptionName String -> Result [([Char], Integer)] -> IO (Result ())
performAction _ (Error tr) = return (Error $ "Trace in configuration argument:" :=> [tr])
performAction args (Content config)
  | member INFO args = infoAction (member PLAIN args) config (args ! INFO)
  | member QUERY args = passKeysToAction args (queryAction (member PLAIN args) config (args ! QUERY))
  | member LIST args = passKeysToAction args (listPairsAction (member PLAIN args) config)
  | member ENCRYPT args = encryptionAction False args procrypt
  | member DECRYPT args = encryptionAction True args procrypt
  | member GENKEYS args = keygenAction (member PLAIN args) (map dropElementInfo config)
  | member GENSPELL args = spellgenAction args
  | member GENNUM args = numgenAction args
  | member GENMOD args = modgenAction args (map dropElementInfo config)
  | member INSPECT args = inspectAction args
  | otherwise = passKeysToAction args (hashAction config)

toIO :: [String] -> IO (Result ()) -> IO ()
toIO rawArgs action = do
  let color
        | "+no-color" `elem` rawArgs = False
        | "+color" `elem` rawArgs = True
        | os == "linux" || os == "linux-android" = True
        | otherwise = False
      errorWord = if color then "\ESC[1;31mError:\ESC[0m" else "ERROR:"
  res <- action
  case res of
    Error tr -> do
      hPutStrLn stderr errorWord
      printTrace [] [False,False] (formatTrace color tr)
      exitWith (ExitFailure 1)
    Content () -> return ()

main :: IO ()
main = do
  rawArgs <- getArgs
  parsedArgs <- parseArgs' (False, False, False) rawArgs
  hSetBuffering stdin NoBuffering
  hSetEcho stdin False
  toIO rawArgs $ raiseH' (raise2' performAction <*> getConfig) parsedArgs
