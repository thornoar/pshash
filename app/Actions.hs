{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use list literal" #-}
module Actions where

import Data.Map (Map, member, (!))
import Data.ByteString (ByteString)
import qualified Data.ByteString as B (readFile, writeFile, putStr, pack, splitAt, append)
import System.Random (getStdGen, randomR, uniformByteString)
import Data.Char (ord)
import System.IO (stderr, hPutStr, hPutChar, hPutStrLn, stdin, hSetEcho)
import System.Info (os)
import Control.Monad (unless, when)

import Algorithm
import Error
import Parsing
import Keys
import Inverse
import Info
import Encryption

currentVersion :: String
currentVersion = "0.1.17.5"

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

readChar :: Bool -> Bool -> Int -> IO String
readChar echo hideNum num = do
  let numstr = "(" ++ show num ++ " letters)"
      lns = length numstr
      bs = replicate lns '\b' ++ replicate lns ' ' ++ replicate lns '\b'
  unless hideNum $ hPutStr stderr numstr
  ch <- getChar
  if ch == '\n' then return ""
  else if ch == '\b' || ch == '\DEL' then do
    unless hideNum $ hPutStr stderr bs
    if num == 0 then readChar echo hideNum num else do
      when echo $ hPutStr stderr "\b \b"
      rest <- readChar echo hideNum (num - 1)
      return (ch : rest)
  else do
    when echo $ hPutChar stderr ch
    unless hideNum $ hPutStr stderr bs
    rest <- readChar echo hideNum (num + 1)
    return $ case rest of
      '\b' : rest' -> rest'
      '\DEL' : rest' -> rest'
      _ -> ch : rest

getInputSimple :: Bool -> Bool -> String -> IO String
getInputSimple echo askRepeat prompt = do
  hSetEcho stdin echo
  unless (null prompt) $ hPutStr stderr prompt
  input <- getLine
  unless echo $ hPutChar stderr '\n'
  if askRepeat then do
    unless (null prompt) $ hPutStr stderr ("(repeat)" ++ replicate (length prompt - 10) ' ' ++ ": ")
    inputRepeat <- getLine
    unless echo $ hPutChar stderr '\n'
    if input == inputRepeat then return input
    else do
      hPutStrLn stderr "Inputs do not match. Try again."
      getInputSimple echo askRepeat prompt
  else return input

getInputFancy :: Bool -> Bool -> String -> IO String
getInputFancy echo askRepeat prompt = do
  hPutStr stderr prompt
  input <- readChar echo (echo || null prompt) 0
  unless (null prompt && not echo) $ hPutChar stderr '\n'
  if askRepeat then do
    unless (null prompt) $ hPutStr stderr ("(repeat)" ++ replicate (length prompt - 10) ' ' ++ ": ")
    inputRepeat <- readChar echo (echo || null prompt) 0
    unless (null prompt && not echo) $ hPutChar stderr '\n'
    if input == inputRepeat then return input
    else do
      unless (null prompt) $ hPutStrLn stderr "Keys do not match. Try again."
      getInputFancy echo askRepeat prompt
  else return input

getKeyStr :: Map OptionName String -> OptionName -> OptionName -> OptionName -> IO String
getKeyStr args opt echoOpt promptOpt
  | member opt args = return $ args ! opt
  | otherwise = getInput echo (member ASKREPEAT args && not echo) (args ! promptOpt)
    where
      echo = member echoOpt args
      getInput = if (os == "linux") then getInputFancy else getInputSimple

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

infoAction :: Bool -> [([Char], Integer)] -> String -> IO (Result ())
infoAction plain config "help" = do
      let show' :: [([Char], Integer)] -> String
          show' config' = "[\n" ++ concatMap ((++ "\n") . ("  " ++) . show) config' ++ "]"    
      putStr . unlines $
          "usage: pshash [ --help | --version | --list | --pure | --impure ]"
        : "              [ --ask-repeat | --show | --plain ]"
        : "              [ --gen-keys | --gen-spell | --gen-num | --gen-mod ]"
        : "              [ +color | +no-color ]"
        : "              [ -k|n|c|i|q|f|p|e|d|r ARGUMENT ]"
        : "              [ ARG_1 ARG_2 ARG_3 ]"
        : if plain then [] else ""
        : "the three arguments ARG_1, ARG_2, ARG_3 can be passed directly on the"
        : "command line, or via standard input. Depending on the options passed"
        : "to pshash, they can have different meanings. Without any flags, these"
        : "arguments are treated as PUBLIC, CHOICE, and SHUFFLE keys, and a"
        : "pseudo-hash is derived from them. Here,"
        : "  PUBLIC              stands for public key, a memorable string indicative"
        : "                      of the password destination (e.g. \"google\", \"steam\")"
        : ""
        : "  CHOICE              stands for choice private key, a large number"
        : ("                      between 0 and 10^" ++ show (getPowerOf 10 (numberOfChoiceKeys' config)))
        : ""
        : "  SHUFFLE             stands for shuffle private key, a number"
        : ("                      between 0 and 10^" ++ show (getPowerOf 10 (numberOfShuffleKeys $ map snd config)))
        : ""
        : "the two keys can each be given in two formats:"
        : "  * arithmetic: an expression with numbers and `^`, `*`, `+`"
        : "    symbols. This expression will be evaluated as usual."
        : "    For example, `6543 + 67^3^2 * 9888 + 23`."
        : "  * mnemonic (spell incantation): a gibberish-looking"
        : "    automatically generated sentence composed from"
        : "    common English syllables, like `mufasa kurimu ro`."
        : ""
        : "options:"
        : "  --help              show this help message and exit"
        : ""
        : "  --version           print the current version of pshash"
        : ""
        : "  --list              print the list of (choice, shuffle) pairs that would"
        : "                      produce the given hash. Treats the three arguments as"
        : "                       * the PUBLIC key,"
        : "                       * the NUMBER of pairs to compute, and"
        : "                       * the final HASH"
        : ""
        : "  --pure              ignore all configuration files, the default behavior"
        : ""
        : "  --impure            enable configuration file usage"
        : ""
        : "  --plain             omit prompts and other auxiliary output when"
        : "                      appropriate"
        : ""
        : "  --ask-repeat        ask the user to repeat keys, which is useful when"
        : "                      generating passwords for the first time"
        : ""
        : "  --show              do not conceal typed keys"
        : ""
        : "  --gen-keys          generate a random choice-shuffle keypair. The"
        : "                      key range depends on the configuration used"
        : ""
        : "  --gen-spell         prompt for a arithmetic key (e.g. `34+78^3` or `456`)"
        : "                      and print the mnemonic spell corresponding to this"
        : "                      key"
        : ""
        : "  --gen-num           prompt for a mnemonic key (e.g. `mufasa` or `begepo`)"
        : "                      and print the numeric value of this key"
        : ""
        : "  --gen-mod           prompt for a key K (in either arithmetic or mnemonic"
        : "                      form) and a number M, printing the value of K modulo"
        : "                      M, both in numeric and mnemonic forms"
        : ""
        : "  +color              enable colors in error messages"
        : ""
        : "  +no-color           disable colors in error messages"
        : ""
        : "                      (options starting with '+' are low-level, they are"
        : "                      parsed at the very end of the execution chain)"
        : ""
        : "  -k KEYWORD          specify the source configuration. KEYWORD can be"
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
        : "  -n \"(L, U, S, D)\"   specify how many Lower case, Upper case,"
        : "                      Special characters, and Digits should be used"
        : ""
        : "  -c CONFIGURATION    specify the source configuration manually,"
        : "                      as the Haskell [([Char], Integer)] type"
        : ""
        : "  -i KEYWORD          show meta information. KEYWORD can be one of:"
        : "                       * help (same as `--help`)"
        : "                       * version (same as `--version`)"
        : "                       * numbers (show the total amounts of hashes/keys)"
        : "                       * times (show times needed to crack your passwords)"
        : ""
        : "                      the output will depend on the source configuration"
        : "                      used"
        : ""
        : "  -q KEYWORD          retrieve one of the keys from a final hash and"
        : "                      two remaining keys. KEYWORD can be one of:"
        : "                       * public (followed by CHOICE SHUFFLE HASH as keys)"
        : "                       * choice (followed by PUBLIC SHUFFLE HASH as keys)"
        : "                       * shuffle (followed by PUBLIC CHOICE HASH as keys)"
        : ""
        : "  -f PATH             read the configuration file from PATH. If neither this"
        : "                      nor the `--pure` option is set, but the `--impure`"
        : "                      option is set, the program will try to read from the"
        : "                      following files:"
        : map ("                       * " ++) defaultConfigFiles ++
          ""
        : "                      each line of the file should follow the format"
        : "                         PUBLIC1, PUBLIC2, ... : ARGS"
        : "                      (other lines will be ignored)"
        : ""
        : "                      a line with the keyword \"+all\" as PUBLIC will apply"
        : "                      to all public keys"
        : ""
        : "                      when using configuration files, the public key needs"
        : "                      to be specified inline as a command line argument."
        : "                      The program will match it with one of the entries in"
        : "                      the file and use the corresponding ARGS"
        : ""
        : "  -p SHIFT            shift all characters in the public key by the"
        : "                      specified amount. This option is generally discouraged,"
        : "                      but sometimes necessary to create multiple passwords"
        : "                      with one set of keys. This option is automatically"
        : "                      suppressed when the `-q` option is used."
        : ""
        : "  -e FILE             encrypt FILE. Accepts three arguments:"
        : "                        * WRITE TO: the file to write the encrypted/decrypted"
        : "                                    data to. A value of `stdout` will write"
        : "                                    to standard output"
        : "                        * KEY 1: first encryption key (e.g. choice key)"
        : "                        * KEY 2: second encryption key (e.g. shuffle key)"
        : ""
        : "  -d FILE             decrypt FILE. Accepts the same arguments"
        : ""
        :("  -r N                use N rounds of encryption. The default is " ++ show defaultRounds)
        : ""
        : "using source configuration:"
        : show' config
        : []
      return (Content ())
infoAction plain _ "version" = do
  unless plain $ putStr "The pshash pseudo-hash password manager, version "
  putStrLn currentVersion
  return (Content ())
infoAction plain config "numbers" =
  let amts = map dropElementInfo config
      numHashes = numberOfHashes amts
      numChoice = numberOfChoiceKeys amts
      numShuffle = numberOfShuffleKeys $ map snd amts
      numRepetitions = numberOfRepetitions $ map snd amts
   in do
  if plain then print numChoice >> print numShuffle >> print numRepetitions
  else putStr $ "\n" ++
    "    symbol distribution : " ++ show amts ++ "\n" ++
    "       number of hashes : " ++ show numHashes ++ " > " ++ printBits numHashes ++ "\n" ++
    "      total hash length : " ++ show ((sum . map snd) amts) ++ " symbols\n\n" ++
    " number of choice  keys : " ++ show numChoice ++ " > " ++ printBits numChoice ++ "\n" ++
    " number of shuffle keys : " ++ show numShuffle ++ " > " ++ printBits numShuffle ++ "\n" ++
    "        hash collisions : " ++ show numRepetitions ++ " > " ++ printBits numRepetitions ++ "\n\n" ++
    "  max public key length : " ++ show (maxLengthOfPublicKey amts) ++ " symbols\n\n"
  return (Content ())
infoAction plain config "times" =
  let amts = map dropElementInfo config
      bfTime = timeToCrack (numberOfHashes amts)
      khTime = timeToCrack (numberOfRepetitions $ map snd amts)
   in do
  if plain then print bfTime >> print khTime
  else putStr $ "\n" ++
    "    symbol distribution : " ++ show amts ++ "\n" ++
    "   assumed attack speed : " ++ "10 billion operations per second\n" ++
    printTimes "  hash brute-force time" bfTime ++ "\n" ++
    printTimes " known hash attack time" khTime ++ "\n\n"
  return (Content ())
infoAction _ _ cmd = return . Error $ ("<Info command not recognized: {{" ++ cmd ++ "}}.>") :=> []

queryAction :: Bool -> [([Char], Integer)] -> String -> [Char] -> String -> String -> IO (Result ())
queryAction plain config kwd arg1 arg2 arg3 =
  let printPublic = if plain then print else \s -> putStr $ "\n public key : " ++ show s ++ "\n\n"
      printPrivate :: Integer -> IO ()
      printPrivate = if plain then print else \n -> putStr $ "\n" ++
        replicate (8 - length kwd) ' ' ++ kwd ++ " key : " ++ show n ++ "\n" ++
        " incantation : " ++ getMnemonic n ++ "\n\n"
   in case kwd of
    "public" -> handleWith printPublic $ retrievePublicKey config arg1 arg2 arg3
    "choice" -> handleWith printPrivate $ retrieveChoiceKey config arg1 arg2 arg3
    "shuffle" -> handleWith printPrivate $ retrieveShuffleKey config arg1 arg2 arg3
    _ -> return . Error $ ("<Query keyword not recognized: \"{{" ++ kwd ++ "}}\".>") :=> []

listPairsAction :: Bool -> [([Char], Integer)] -> String -> String -> [Char] -> IO (Result ())
listPairsAction plain config publicStr limitStr hashStr =
  let mnc = numberOfChoiceKeys' config
      mns = numberOfShuffleKeys' config
      publicKey = getPublicKey publicStr
      mlimit = readResult "integer" limitStr :: Result Integer
      sequence' :: [IO (Result ())] -> IO (Result ())
      sequence' [] = return (Content ())
      sequence' (io : rest) = io >>= \res -> case res of
        Error tr -> return (Error tr)
        Content () -> sequence' rest
   in case mlimit of
        Error tr -> return (Error $ "Trace while reading number of pairs to print:" :=> [tr])
        Content limit -> if plain then do
          let format :: Integer -> Integer -> String
              format shuffleKey preChoiceKey = show (mod (preChoiceKey - publicKey) mnc) ++ " " ++ show shuffleKey
              getPair :: Integer -> Result String
              getPair shuffleKey = fmap (format shuffleKey) (getHashI' config hashStr shuffleKey)
          sequence' $ map (handleWith putStrLn  . getPair) [0 .. min limit mns - 1]
        else do
          let ml = max 11 $ 2 + length (show mnc)
              sl = max 12 $ length (show mns)
              format :: Integer -> Integer -> String
              format shuffleKey preChoiceKey =
                let nstr = show $ mod (preChoiceKey - publicKey) (numberOfChoiceKeys' config)
                 in " " ++ nstr ++ replicate (ml - length nstr) ' ' ++ "| " ++ show shuffleKey
              getPair :: Integer -> Result String
              getPair shuffleKey = fmap (format shuffleKey) (getHashI' config hashStr shuffleKey)
          putStrLn $
            "\n choice key" ++ replicate (ml - 10) ' ' ++ "| shuffle key\n " ++
            replicate ml '-' ++ "+" ++ replicate sl '-'
          res <- sequence' $ map (handleWith putStrLn  . getPair) [0 .. min limit mns - 1]
          putStrLn ""
          return res

keygenAction :: Bool -> [(Integer, Integer)] -> IO (Result ())
keygenAction plain amts = do
  g <- getStdGen
  let choice = fst $ randomR (0, numberOfChoiceKeys amts) g :: Integer
      shuffle = fst $ randomR (0, numberOfShuffleKeys $ map snd amts) g :: Integer
  if plain then print choice >> print shuffle
  else putStr $ "\n" ++
    "  choice key : " ++ show choice ++ "\n" ++
    " incantation : " ++ getMnemonic choice ++ "\n\n" ++
    " shuffle key : " ++ show shuffle ++ "\n" ++
    " incantation : " ++ getMnemonic shuffle ++ "\n\n"
  return (Content ())

spellgenAction :: Map OptionName String -> IO (Result ())
spellgenAction args = do
  key <- getKeyStr args FIRST E1 P1
  case getPrivateKeyNum key of
    Error tr -> return $ Error $ "Trace while generating mnemonic incantation:" :=> [tr]
    Content n -> do
      if (member PLAIN args) then putStrLn (getMnemonic n)
      else putStr $ "\n incantation : " ++ getMnemonic n ++ "\n\n"
      return (Content ())

numgenAction :: Map OptionName String -> IO (Result ())
numgenAction args = do
  mnem <- getKeyStr args FIRST E1 P1
  case getPrivateKeyMnemonic mnem of
    Error tr -> return $ Error $ "Trace while generating numeric key:" :=> [tr]
    Content k -> do
      if (member PLAIN args) then print k
      else putStr $ "\n numeric key : " ++ show k ++ "\n\n"
      return (Content ())

modgenAction :: Map OptionName String -> [(Integer, Integer)] -> IO (Result ())
modgenAction args amts = do
  choiceStr <- getKeyStr args FIRST E1 P1
  shuffleStr <- getKeyStr args SECOND E2 P2
  case (getPrivateKey choiceStr, getPrivateKey shuffleStr) of
    (Error tr1, Error tr2) -> return $ Error $ "Double trace while performing modulus operation:" :=> [tr1, tr2]
    (Error tr, _) -> return $ Error $ "Trace while performing modulus operation, reading the choice key:" :=> [tr]
    (_, Error tr) -> return $ Error $ "Trace while performing modulus operation, reading the shuffle key:" :=> [tr]
    (Content choice, Content shuffle) -> do
      let choiceSpr = numberOfChoiceKeys amts
          shuffleSpr = numberOfShuffleKeys (map snd amts)
          newChoice = mod choice choiceSpr
          newShuffle = mod shuffle shuffleSpr
      if member PLAIN args then print newChoice >> print newShuffle
      else putStr $ "\n" ++
        " symbol distribution : " ++ show amts ++ "\n" ++
        "  choice key modulus : " ++ show choiceSpr ++ "\n" ++
        " shuffle key modulus : " ++ show shuffleSpr ++ "\n\n" ++
        "  shorter choice key : " ++ show newChoice ++ "\n" ++
        "         incantation : " ++ getMnemonic newChoice ++ "\n\n" ++
        " shorter shuffle key : " ++ show newShuffle ++ "\n" ++
        "         incantation : " ++ getMnemonic newShuffle ++ "\n\n"
      return (Content ())

encryptionAction ::
  Bool ->
  Map OptionName String ->
  (Int -> (ByteString, ByteString) -> Integer -> Integer -> ByteString) ->
  IO (Result ())
encryptionAction dec args func = do
  let mrounds = if member ROUNDS args then readResult "integer" (args ! ROUNDS) else Content defaultRounds
      fname = args ! if dec then DECRYPT else ENCRYPT
  outfile <- getKeyStr args FIRST E1 P1
  mkey1 <- getPrivateKey <$> getKeyStr args SECOND E2 P2
  mkey2 <- getPrivateKey <$> getKeyStr args THIRD E3 P3
  mcts <- case fname of
    "stdin" -> fmap (Content . B.pack . map (fromIntegral . ord)) getContents
    _ -> readFileResult B.readFile fname
  let write = if outfile == "stdout" then B.putStr else B.writeFile outfile
  g <- getStdGen
  handleWith write $ case (mrounds, mkey1, mkey2, mcts) of
    (Error tr, _, _, _) -> Error $ (pref ++ "{number of rounds}:") :=> [tr]
    (_, Error tr, _, _) -> Error $ (pref ++ "{first key}:") :=> [tr]
    (_, _, Error tr, _) -> Error $ (pref ++ "{second key}:") :=> [tr]
    (_, _, _, Error tr) -> Error $ (pref ++ "{plaintext}:") :=> [tr]
    (Content rounds, Content k1, Content k2, Content msg) ->
      let (iv, msg') = if dec then B.splitAt defaultSize msg else (fst $ uniformByteString defaultSize g, msg)
      in Content $ (if dec then id else B.append iv) $ func rounds (iv,msg') k1 k2
    where pref = "Trace while performing encryption/decryption, reading the "

hashAction :: [([Char], Integer)] -> String -> String -> String -> IO (Result ())
hashAction config publicStr choiceStr shuffleStr = handleWith putStrLn $ getFinalHash config publicStr choiceStr shuffleStr
