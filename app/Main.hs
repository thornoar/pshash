module Main where

import Data.Map (Map, member, (!))
import System.Environment (getArgs)

import Error
import Actions
import Parsing
import qualified Data.Map as DM

-- ┌────────────────┐
-- │ USER INTERFACE │
-- └────────────────┘

performAction :: Map OptionName String -> IO (Result ())
performAction args
  | member INFO args = addTrace "Getting meta information:" <$> passConfig (DM.lookup FIRST args) args (const $ infoAction (member PLAIN args) (args ! INFO))
  | member QUERY args = addTrace "Performing a query operation:" <$> passInputs args (queryAction (member PLAIN args) (args ! QUERY))
  | member LIST args = addTrace "Listing key pairs:" <$> passInputsWithConfig args (listPairsAction (member PLAIN args))
  | member ENCRYPT args = addTrace "Decrypting file:" <$> encryptionAction False args
  | member DECRYPT args = addTrace "Encrypting file:" <$> encryptionAction True args
  | member LOOP args = addTrace "Running pseudo-hash generation loop:" <$> loopAction args
  | member GENKEYS args = addTrace "Generating private keys:" <$> passConfig (DM.lookup FIRST args) args (const $ keygenAction (member PLAIN args))
  | member GENSPELL args = addTrace "Producing a mnemonic incantation:" <$> spellgenAction args
  | member GENNUM args = addTrace "Converting an incantation to numeric form:" <$> numgenAction args
  | member GENMOD args = addTrace "Performing modular reduction on keys:" <$> passConfig (DM.lookup FIRST args) args modgenAction
  | member INSPECT args = addTrace "Inspecting configuration file:" <$> inspectAction args
  | otherwise = addTrace "Computing pseudo-hash:" <$> passInputsWithConfig args hashAction

main :: IO ()
main = do
  rawArgs <- getArgs
  let parsedArgs = addTrace "Parsing command-line options:" $ parseArgs (False, False, False) rawArgs
  toIO rawArgs $ handleWithM performAction (setEchoesAndPrompts <$> parsedArgs)
