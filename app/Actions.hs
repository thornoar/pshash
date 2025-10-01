{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use list literal" #-}
module Actions where

import Data.Map (Map, member, (!))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS (readFile, writeFile, putStr, pack)
import System.Random (getStdGen, randomR)
import Data.Char (ord)
import System.IO (stdin, stderr, hGetEcho, hSetEcho, hPutStr, hPutChar, hPutStrLn)
import Control.Exception (bracket_)
import Control.Monad (unless)

import Algorithm
import Error
import Parsing
import Keys
import Inverse
import Info
import Encryption

currentVersion :: String
currentVersion = "0.1.16.10"

-- ┌─────────────────────┐
-- │ FINAL HASH FUNCTION │
-- └─────────────────────┘

getFinalHash :: [([Char], Integer)] -> String -> String -> String -> Result [Char]
getFinalHash config publicStr choiceStr shuffleStr =
  liftH2
    ("Trace while computing hash, getting the " ++ "{choice}" ++ " key:")
    ("Trace while computing hash, getting the " ++ "{shuffle}" ++ " key:")
    (getHash config) choiceKey shuffleKey
  where
    publicKey = getPublicKey publicStr
    choiceKey = liftA2 mod ((+publicKey) <$> getPrivateKey choiceStr) $ return (chooseAndMergeSpread' config)
    shuffleKey = getPrivateKey shuffleStr

-- ┌──────────────────┐
-- │ HELPER FUNCTIONS │
-- └──────────────────┘

withEcho :: Bool -> IO a -> IO a
withEcho echo action = do
  old <- hGetEcho stdin
  bracket_ (hSetEcho stdin echo) (hSetEcho stdin old) action

getInput :: Bool -> Bool -> String -> IO String
getInput echo askRepeat prompt = do
  unless (null prompt) $ hPutStr stderr prompt
  input <- withEcho echo getLine
  unless echo $ hPutChar stderr '\n'
  if askRepeat then do
    unless (null prompt) $ hPutStr stderr ("(repeat)" ++ replicate (length prompt - 10) ' ' ++ ": ")
    inputRepeat <- withEcho echo getLine
    unless echo $ hPutChar stderr '\n'
    if input == inputRepeat then return input
    else do
      hPutStrLn stderr "Keys do not match. Try again."
      getInput echo askRepeat prompt
  else return input

getKeyStr :: Map OptionName String -> OptionName -> OptionName -> OptionName -> IO String
getKeyStr args opt echoOpt promptOpt
  | member opt args = return $ args ! opt
  | otherwise = getInput (member echoOpt args) (member ASKREPEAT args) (args ! promptOpt)

-- ┌─────────────────┐
-- │ QUERY FUNCTIONS │
-- └─────────────────┘

retrievePublicKey :: [([Char], Integer)] -> String -> String -> [Char] -> Result String
retrievePublicKey config choiceStr shuffleStr hashStr =
  let shuffleKey = getPrivateKey shuffleStr
      preChoiceKey = getPrivateKey choiceStr
      choiceKey = shuffleKey >>= getHashI' config hashStr
   in addTrace "Trace while retrieving public key:" $
      getPublicStr <$> fmap2 mod (liftA2 (-) choiceKey preChoiceKey) (numberOfPublicKeys' config)

retrieveChoiceKey :: [([Char], Integer)] -> String -> String -> [Char] -> Result Integer
retrieveChoiceKey config publicStr shuffleStr hashStr =
  let publicKey = getPublicKey publicStr
      shuffleKey = getPrivateKey shuffleStr
      preChoiceKey = shuffleKey >>= getHashI' config hashStr
      choiceMergeSpr = chooseAndMergeSpread' config
   in addTrace "Trace while retrieving choice key:" $
      fmap2 mod (fmap2 (-) preChoiceKey publicKey) choiceMergeSpr

retrieveShuffleKey :: [([Char], Integer)] -> String -> String -> [Char] -> Result Integer
retrieveShuffleKey config publicStr choiceStr hashStr =
  let publicKey = getPublicKey publicStr
      preChoiceKey = getPrivateKey choiceStr
      choiceKey = fmap2 mod (fmap (+ publicKey) preChoiceKey) (numberOfChoiceKeys' config)
      preHash = fmap (chooseAndMerge config) choiceKey
   in addTrace "Trace while retrieving shuffle key:" $
      raise2 shuffleListI preHash hashStr

-- ┌─────────┐
-- │ ACTIONS │
-- └─────────┘

handleWith :: (a -> IO ()) -> Result a -> IO (Result ())
handleWith f ma = case ma of
  Error tr -> return (Error tr)
  Content a -> f a >> return (Content ())

infoAction :: [([Char], Integer)] -> String -> IO (Result ())
infoAction config "help" = do
      let show' :: [([Char],Integer)] -> String
          show' config' = "[\n" ++ concatMap ((++ "\n") . ("  " ++) . show) config' ++ "]"
      putStrLn . unlines $
          "usage: pshash [ --help | --version | --list | --pure | --impure ]"
        : "              [ --no-prompts | --ask-repeat | --show | --gen-keys ]"
        : "              [ +color | +no-color ]"
        : "              [ -k|n|c|i|q|f|p|e|r ARGUMENT ]"
        : "              [ PUBLIC CHOICE SHUFFLE ]"
        : ""
        : "options:"
        : "  --help              Show this help message and exit"
        : ""
        : "  --version           Print the current version of pshash"
        : ""
        : "  --list              Print the list of (choice,shuffle) pairs that would"
        : "                      produce the given hash. Accepts three arguments:"
        : "                       * the PUBLIC key,"
        : "                       * the NUMBER of pairs to compute, and"
        : "                       * the final HASH."
        : ""
        : "  --pure              Ignore all configuration files. The default behavior."
        : ""
        : "  --impure            Enable configuration file usage."
        : ""
        : "  --no-prompts        Omit prompts."
        : ""
        : "  --ask-repeat        Ask the user to repeat keys."
        : ""
        : "  --show              Do not conceal typed keys."
        : ""
        : "  --gen-keys          Generate a random primary-secondary keypair. The"
        : "                      key range depends on the configuration used."
        : ""
        : "  +color              Enable colors in error messages."
        : ""
        : "  +no-color           Disable colors in error messages."
        : ""
        : "                      (options starting with '+' are low-level, they are"
        : "                      parsed at the very end of the execution chain)"
        : ""
        : "  -k KEYWORD          Specify the source configuration. KEYWORD can be"
        : "                      one of the following (default is long):"
        : "                       * long (8 upper, 8 lower, 5 special, 4 digits)"
        : "                       * medium (5 symbols of each type above)"
        : "                       * short (4 symbols of each type)"
        : "                       * anlong (7 upper case, 7 lower case, 7 digits)"
        : "                       * anshort (4 upper case, 4 lower case, 4 digits)"
        : "                       * pin (4-digit pin code)"
        : "                       * mediumpin (6-digit pin code)"
        : "                       * longpin (8-digit pin code)"
        : ""
        : "  -n \"(L, U, S, D)\"   Specify how many Lower case, Upper case,"
        : "                      Special characters, and Digits should be used"
        : ""
        : "  -c CONFIGURATION    Specify the source configuration manually,"
        : "                      as Haskell [([Char], Integer)] type"
        : ""
        : "  -i KEYWORD          Show meta information. KEYWORD can be one of:"
        : "                       * help (same as `--help`)"
        : "                       * version (same as `--version`)"
        : "                       * numbers (show the total amounts of hashes/keys)"
        : "                       * times (show times needed to crack your passwords)"
        : ""
        : "  -q KEYWORD          Retrieve one of the keys from a final hash and"
        : "                      two remaining keys. KEYWORD can be one of:"
        : "                       * public (followed by CHOICE SHUFFLE HASH as keys)"
        : "                       * choice (followed by PUBLIC SHUFFLE HASH as keys)"
        : "                       * shuffle (followed by PUBLIC CHOICE HASH as keys)"
        : ""
        : "  -f PATH             Read the configuration file from PATH. If neither this"
        : "                      nor the `--pure` option is used, but the `--impure`"
        : "                      option is set, the program will try to read from the"
        : "                      following files:"
        : map ("                       * " ++) defaultConfigFiles ++
          "                      Each line of the file should follow the format"
        : "                         PUBLIC1, PUBLIC2, ... : ARGS"
        : "                      (other lines will be ignored)"
        : "                      A line with the keyword \"+all\" as PUBLIC will apply"
        : "                      to all public keys."
        : "                      When using configuration files, the public key needs"
        : "                      to be specified inline as a command line argument."
        : "                      The program will match it with one of the entries in"
        : "                      the file and use the corresponding ARGS."
        : ""
        : "  -p SHIFT            Shift all characters in the public key by the"
        : "                      specified amount. This option is generally discouraged,"
        : "                      but sometimes necessary to create multiple passwords"
        : "                      with one set of keys. This option is automatically"
        : "                      suppressed when the `-q` option is used."
        : ""
        : "  -e FILE             Encrypt/decrypt FILE, writing encrypted data to stdout."
        : "                      Accepts three arguments:"
        : "                        * WRITE TO: the file to write the encrypted/decrypted"
        : "                                    data to. A value of `stdout` will write"
        : "                                    to stdout."
        : "                        * KEY 1: first encryption key (e.g. choice key)."
        : "                        * KEY 2: second encryption key (e.g. shuffle key)."
        : "                      The encryption and decryption algorithms are the same."
        : ""
        :("  -r N                Use N rounds of encryption. The default is " ++ show defaultRounds ++ ".")
        : ""
        : "main arguments:"
        : "  PUBLIC              Stands for public key, a memorable string indicative"
        : "                      of the password destination (e.g. \"google\", \"steam\")"
        : ""
        : "  CHOICE              Stands for choice private key, a large number"
        : ("                      between 0 and 10^" ++ show (getPowerOf 10 (numberOfChoiceKeys' config)))
        : ""
        : "  SHUFFLE             Stands for shuffle private key, a number"
        : ("                      between 0 and 10^" ++ show (getPowerOf 10 (numberOfShuffleKeys $ map snd config)))
        : ""
        : "using source configuration:"
        : show' config
        : []
      return (Content ())
infoAction _ "version" = putStrLn ("The pshash pseudo-hash password manager, version " ++ currentVersion) >> return (Content ())
infoAction config "numbers" =
  let amts = map dropElementInfo config
      numHashes = numberOfHashes amts
      numChoice = numberOfChoiceKeys amts
      numShuffle = numberOfShuffleKeys $ map snd amts
      numRepetitions = numberOfRepetitions $ map snd amts
   in do
  putStrLn $ "using the following configuration distribution: " ++ show amts
  putStrLn ""
  putStrLn $ "total theoretical number of hashes:         > " ++ printBits numHashes
  putStrLn $ "number of choice keys:                      > " ++ printBits numChoice
  putStrLn $ "number of shuffle keys:                     > " ++ printBits numShuffle
  putStrLn $ "number of key pairs with the same hash:     > " ++ printBits numRepetitions
  putStrLn $ "total hash length:                          " ++ show ((sum . map snd) amts) ++ " symbols"
  putStrLn $ "maximum relevant length of the public key:  " ++ show (maxLengthOfPublicKey amts) ++ " symbols"
  return (Content ())
infoAction config "times" = let amts = map dropElementInfo config in do
  putStrLn $ "using the following configuration distribution: " ++ show amts
  putStrLn $ "assumed number of password checks per second:   " ++ "10 billion = 10^10"
  putStrLn $ "time to check one password:                     " ++ "10^(" ++ show timeToCheckPower ++ ") s = " ++ show timeToCheckPicos ++ " picoseconds"
  putStrLn ""
  putStrLn $ printTimes "time to brute-force your password:              " (timeToCrack $ numberOfHashes amts)
  putStrLn ""
  putStrLn $ printTimes "time to retrieve the keys based on a hash:      " (timeToCrack $ numberOfRepetitions $ map snd amts)
  return (Content ())
infoAction _ cmd = return . Error $ ("<Info command not recognized: {{" ++ cmd ++ "}}.>") :=> []

queryAction :: [([Char], Integer)] -> String -> [Char] -> String -> String -> IO (Result ())
queryAction config "public" choiceStr shuffleStr hashStr = handleWith print $ retrievePublicKey config choiceStr shuffleStr hashStr
queryAction config "choice" publicStr shuffleStr hashStr = handleWith print $ retrieveChoiceKey config publicStr shuffleStr hashStr
queryAction config "shuffle" publicStr choiceStr hashStr = handleWith print $ retrieveShuffleKey config publicStr choiceStr hashStr
queryAction _ kw _ _ _ = return . Error $ ("<Query keyword not recognized: {{" ++ kw ++ "}}.>") :=> []

listPairsAction :: [([Char], Integer)] -> String -> String -> [Char] -> IO (Result ())
listPairsAction config publicStr limitStr hashStr =
  let publicKey = getPublicKey publicStr
      mlimit = readResult "integer" limitStr :: Result Integer
      format :: Integer -> Integer -> String
      format shuffleKey preChoiceKey = show (mod (preChoiceKey - publicKey) (numberOfChoiceKeys' config)) ++ " " ++ show shuffleKey
      getPair :: Integer -> Result String
      getPair shuffleKey = fmap (format shuffleKey) (getHashI' config hashStr shuffleKey)
      sequence' :: [IO (Result ())] -> IO (Result ())
      sequence' [] = return (Content ())
      sequence' (io : rest) = do
        res <- io
        case res of
          Error tr -> return (Error tr)
          Content () -> sequence' rest
   in case mlimit of
        Error tr -> return (Error $ "Trace while reading number of pairs to print:" :=> [tr])
        Content limit -> sequence' $ take' limit $ map (handleWith putStrLn  . getPair) [0 .. numberOfShuffleKeys' config - 1]

keygenAction :: [(Integer, Integer)] -> IO (Result ())
keygenAction amts = do
  g <- getStdGen
  let choice = fst $ randomR (0, numberOfChoiceKeys amts) g :: Integer
      shuffle = fst $ randomR (0, numberOfShuffleKeys $ map snd amts) g :: Integer
      printMnems :: [String] -> IO ()
      printMnems lst = if null lst then putStrLn "    (no mnemonics available)" else do
        putStrLn "mnemonics: "
        mapM_ putStrLn $ zipWith (++) ("mnemonics " : repeat "          ") lst
  putStrLn $ "private choice key:  " ++ show choice
  chmnems <- getMnemonics defaultMnemonicAmount (show choice)
  printMnems chmnems
  putStrLn $ "private shuffle key: " ++ show shuffle
  shmnems <- getMnemonics defaultMnemonicAmount (show choice)
  printMnems shmnems
  return (Content ())

encryptionAction ::
  Map OptionName String ->
  (Int -> ByteString -> Integer -> Integer -> ByteString) ->
  FilePath ->
  IO (Result ())
encryptionAction args func fname = do
  let mrounds = if member ROUNDS args then readResult "integer" (args ! ROUNDS) else Content defaultRounds
  outfile <- getKeyStr args FIRST E1 P1
  mkey1 <- getPrivateKey <$> getKeyStr args SECOND E2 P2
  mkey2 <- getPrivateKey <$> getKeyStr args THIRD E3 P3
  mcts <- case fname of
    "stdin" -> fmap (Content . BS.pack . map (fromIntegral . ord)) getContents
    _ -> readFileResult BS.readFile fname
  let write = if outfile == "stdout" then BS.putStr else BS.writeFile outfile
  handleWith write $ case (mrounds, mkey1, mkey2, mcts) of
    (Error tr, _, _, _) -> Error $ (pref ++ "{number of rounds}:") :=> [tr]
    (_, Error tr, _, _) -> Error $ (pref ++ "{first key}:") :=> [tr]
    (_, _, Error tr, _) -> Error $ (pref ++ "{second key}:") :=> [tr]
    (_, _, _, Error tr) -> Error $ (pref ++ "{plaintext}:") :=> [tr]
    (Content rounds, Content k1, Content k2, Content msg) -> Content $ func rounds msg k1 k2
    where pref = "Trace while performing encryption/decryption, reading the "

hashAction :: [([Char], Integer)] -> String -> String -> String -> IO (Result ())
hashAction config publicStr choiceStr shuffleStr = handleWith putStrLn $ getFinalHash config publicStr choiceStr shuffleStr
