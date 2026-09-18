module Main where

import System.IO (stderr, hPutStrLn)
import Data.Map (Map, member, (!))
import System.Environment (getArgs)
import System.Info (os)
import System.Exit (exitWith, ExitCode (ExitFailure))

import Algorithm
import Error
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
  public <- getKeyStr args FIRST E1 P1
  -- let publicPatched
  --       | member PATCH args = flip shiftString public <$> (readResult "integer" (args ! PATCH) :: Result Integer)
  --       | otherwise = Content public
  second <- getKeyStr args SECOND E2 P2
  third <- getKeyStr args THIRD E3 P3
  act public second third

performAction :: Map OptionName String -> [([Char], Integer)] -> IO (Result ())
performAction args config
  | member INFO args = addTrace "Getting meta information:" <$> infoAction (member PLAIN args) config (args ! INFO)
  | member QUERY args = addTrace "Performing a query operation:" <$> passKeysToAction args (queryAction (member PLAIN args) config (args ! QUERY))
  | member LIST args = addTrace "Listing key pairs:" <$> passKeysToAction args (listPairsAction (member PLAIN args) config)
  | member ENCRYPT args = addTrace "Decrypting file:" <$> encryptionAction False args
  | member DECRYPT args = addTrace "Encrypting file:" <$> encryptionAction True args
  -- | member LOOP args = addTrace "Running pseudo-hash generation loop:" <$> loopAction args config
  | member GENKEYS args = addTrace "Generating private keys:" <$> keygenAction (member PLAIN args) (map dropElementInfo config)
  | member GENSPELL args = addTrace "Producing a mnemonic incantation:" <$> spellgenAction args
  | member GENNUM args = addTrace "Converting an incantation to numeric form:" <$> numgenAction args
  | member GENMOD args = addTrace "Performing modular reduction on keys:" <$> modgenAction args (map dropElementInfo config)
  | member INSPECT args = addTrace "Inspecting configuration file:" <$> inspectAction args
  | otherwise = addTrace "Computing pseudo-hash:" <$> passKeysToAction args (hashAction config)

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
    Error trs -> do
      hPutStrLn stderr errorWord
      printTraceList [] (map (formatTrace color) trs)
      exitWith (ExitFailure 1)
    Content () -> return ()

main :: IO ()
main = do
  rawArgs <- getArgs
  parsedArgs <- parseArgs' rawArgs
  toIO rawArgs $ handleWithM (raise2' performAction <*> addTrace "Reading the source configuration:" . getConfig) parsedArgs
