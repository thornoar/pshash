{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use list literal" #-}
{-# LANGUAGE FlexibleInstances #-}
module Actions where

import Data.Map (Map, member, (!), unionWith)
import qualified Data.Map as DM
import Data.ByteString (fromStrict)
import qualified Data.ByteString.Lazy as B (readFile, writeFile, putStr, pack, splitAt, append)
import System.Random (getStdGen, randomR, genByteString)
import Data.Char (ord)
import System.IO (stderr, hPutStr, hPutChar, hPutStrLn, stdin, hSetEcho, BufferMode (NoBuffering), hSetBuffering)
import System.Info (os)
import Control.Monad (unless, when)

import Algorithm
import Error
import Parsing
import Keys
import Inverse
import Info
import Encryption
import System.Directory (getHomeDirectory)
import System.Environment (lookupEnv)
import System.Process (readProcess, callProcess)
import Control.Exception (catch, SomeException)
import System.Exit (ExitCode)
import System.IO.Error (isDoesNotExistError, isPermissionError)
-- import GHC.IO.Exception (IOException(IOError))

currentVersion :: String
currentVersion = "0.1.22.0"

-- ┌─────────────────────┐
-- │ FINAL HASH FUNCTION │
-- └─────────────────────┘

getFinalHash :: Config -> String -> String -> String -> Result [Char]
getFinalHash config publicStr choiceStr shuffleStr =
  (getHash config) <$>
  (addTrace ("Reading the " ++ "{choice}" ++ " key:") choiceKey) <*>
  (addTrace ("Reading the " ++ "{shuffle}" ++ " key:") shuffleKey)
  where
    publicKey = getPublicKey publicStr
    choiceKey = fmap2 mod ((+publicKey) <$> getPrivateKey choiceStr) (chooseAndMergeSpread' config)
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
  let chord = ord ch
  if ch == '\n' then return ""
  else if ch == '\b' || ch == '\DEL' then do
    unless hideNum $ hPutStr stderr bs
    if num == 0 then readChar echo hideNum num else do
      when echo $ hPutStr stderr "\b \b"
      rest <- readChar echo hideNum (num - 1)
      return (ch : rest)
  else if
    (chord >= 97 && chord <= 122) ||
    (chord >= 48 && chord <= 57) ||
    (chord >= 65 && chord <= 90) ||
    elem ch sourceSpecial ||
    (echo && elem ch [' ', '[', ']', '(', ')', ',', '-', '\\', '\'', '\"', ':'])
  then do
    when echo $ hPutChar stderr ch
    unless hideNum $ hPutStr stderr bs
    rest <- readChar echo hideNum (num + 1)
    return $ case rest of
      '\b' : rest' -> rest'
      '\DEL' : rest' -> rest'
      _ -> ch : rest
  else do
    unless hideNum $ hPutStr stderr bs
    readChar echo hideNum num

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
  hSetBuffering stdin NoBuffering
  hSetEcho stdin False
  hPutStr stderr prompt
  input <- readChar echo (echo || null prompt) 0
  unless (null prompt && not echo) $ hPutChar stderr '\n'
  if askRepeat then do
    unless (null prompt) $ hPutStr stderr ("(repeat)" ++ replicate (length prompt - 10) ' ' ++ ": ")
    inputRepeat <- readChar echo (echo || null prompt) 0
    unless (null prompt && not echo) $ hPutChar stderr '\n'
    if input == inputRepeat then return input
    else do
      unless (null prompt) $ hPutStrLn stderr "Inputs do not match. Try again."
      getInputFancy echo askRepeat prompt
  else return input

getInput :: Bool -> Bool -> String -> IO String
getInput = if (os == "linux") then getInputFancy else getInputSimple

getKeyStr :: Map OptionName String -> OptionName -> OptionName -> OptionName -> IO String
getKeyStr args opt echoOpt promptOpt
  | member opt args = return $ args ! opt
  | otherwise = getInput echo (member ASKREPEAT args && not echo) (args ! promptOpt)
    where
      echo = member echoOpt args

retrievePublicKey :: Config -> String -> String -> [Char] -> Result String
retrievePublicKey config choiceStr shuffleStr hashStr =
  let shuffleKey = getPrivateKey shuffleStr
      preChoiceKey = getPrivateKey choiceStr
      choiceKey = shuffleKey >>= getHashI' config hashStr
   in fmap2 (getPublicStr <.> mod) (liftA2 (-) choiceKey preChoiceKey) (numberOfPublicKeys' config)

retrieveChoiceKey :: Config -> String -> String -> [Char] -> Result Integer
retrieveChoiceKey config publicStr shuffleStr hashStr =
  let publicKey = getPublicKey publicStr
      shuffleKey = getPrivateKey shuffleStr
      preChoiceKey = shuffleKey >>= getHashI' config hashStr
      choiceMergeSpr = chooseAndMergeSpread' config
   in fmap2 mod (fmap2 (-) preChoiceKey publicKey) choiceMergeSpr

retrieveShuffleKey :: Config -> String -> String -> [Char] -> Result Integer
retrieveShuffleKey config publicStr choiceStr hashStr =
  let publicKey = getPublicKey publicStr
      preChoiceKey = getPrivateKey choiceStr
      choiceKey = fmap2 mod (fmap (+ publicKey) preChoiceKey) (numberOfChoiceKeys' config)
      preHash = fmap (chooseAndMerge config) choiceKey
   in bind2 shuffleListI preHash hashStr

-- ┌────────────────┐
-- │ ACTION HELPERS │
-- └────────────────┘

passConfig :: Maybe String -> Map OptionName String -> (Map OptionName String -> Config -> IO (Result a)) -> IO (Result a)
passConfig mpub args act = do
  mnewArgs <- addConfigArgs args mpub
  handleWithMsgM' "Adding arguments from config file:" (setEchoesAndPrompts <$> mnewArgs) $ \newArgs ->
    handleWithMsgM' "Parsing source configuration:" (getConfig newArgs) (act newArgs)

passInputs ::
  Map OptionName String ->
  (Map OptionName String -> String -> String -> String -> IO (Result ())) ->
  IO (Result ())
passInputs args act = do
  first <- getKeyStr args FIRST E1 P1
  second <- getKeyStr args SECOND E2 P2
  third <- getKeyStr args THIRD E3 P3
  act args first second third

passInputsWithConfig ::
  Map OptionName String ->
  (Config -> String -> String -> String -> IO (Result ())) ->
  IO (Result ())
passInputsWithConfig args act = do
  public <- getKeyStr args FIRST E1 P1
  mnewArgs <- addConfigArgs args (Just public)
  handleWithMsgM' "Adding arguments from config file:" (setEchoesAndPrompts <$> mnewArgs) $ \newArgs -> do
    second <- getKeyStr newArgs SECOND E2 P2
    third <- getKeyStr newArgs THIRD E3 P3
    handleWithMsgM' "Applying public key patch:" (patchString newArgs False public) $ \public' ->
      handleWithMsgM' "Parsing source configuration:" (getConfig newArgs) $ \config ->
      act config public' second third

-- ┌──────────────────────┐
-- │ CLIPBOARD AND OUTPUT │
-- └──────────────────────┘

setClipboardUnsafe :: String -> IO (Result ())
setClipboardUnsafe text = case os of
  "linux" -> do
    isWayland <- lookupEnv "WAYLAND_DISPLAY"
    case isWayland of
      Nothing -> readProcess "xclip" ["-selection", "clipboard", "-i"] text >> return ()
      Just _ -> callProcess "wl-copy" [text]
    return (Content ())
  "mingw32" -> readProcess "clip.exe" [] text >> return (Content ())
  _ -> return $ Error [("<Clipboard output is not supported on {{" ++ os ++ "}}.>") :=> []]

setClipboardSafe :: String -> IO (Result ())
setClipboardSafe text = setClipboardUnsafe text `catch` handleExitCode `catch` handleIOError `catch` catchAll
  where
    handleExitCode :: ExitCode -> IO (Result ())
    handleExitCode _ = return $ Error ["<Clipboard command exited with non-zero code.>" :=> []]
    handleIOError :: IOError -> IO (Result ())
    handleIOError e
      | isDoesNotExistError e = return $ Error ["<Clipboard command (wl-copy/xclip/clip.exe) could not be found.>" :=> []]
      | isPermissionError e   = return $ Error ["<Permission denied when running the clipboard command.>" :=> []]
      | otherwise             = return $ Error ["OS/IO Error:" :=> [show e :=> []]]
    catchAll :: SomeException -> IO (Result ())
    catchAll _ = return $ Error ["<An error occured while trying to set the system clipboard.>" :=> []]

outputStrLn :: Bool -> String -> IO (Result ())
outputStrLn True = fmap (addTrace "Sending text to the clipboard:") . setClipboardSafe
outputStrLn False = fmap Content . putStrLn

-- ┌─────────┐
-- │ ACTIONS │
-- └─────────┘

infoAction :: Bool -> Bool -> String -> Config -> IO (Result ())
infoAction plain clip "help" config = do
      let show' :: Config -> String
          show' config' = "[\n" ++ concatMap ((++ "\n") . ("  " ++) . show) config' ++ "]"
      outputStrLn clip . unlines $
          "usage: pshash [ --help | --version | --list | --inspect | --loop | --clip ]"
        : "              [ --gen-keys | --gen-spell | --gen-num | --gen-mod ]"
        : "              [ --ask-repeat | --show | --plain ]"
        : "              [ --pure | --impure ]"
        : "              [ +color | +no-color ]"
        : "              [ -k|n|c|i|q|f|p|e|d|r ARG ]"
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
        : "the two private keys can each be given in two formats:"
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
        : "                      produce the given pseudo-hash. Treats the arguments as"
        : "                       * the PUBLIC key,"
        : "                       * the NUMBER of pairs to compute, and"
        : "                       * the final HASH"
        : "                      (this option does not support --clip)"
        : ""
        : "  --inspect           format and print the contents of a configuration file"
        : "                      (this option does not support --clip)"
        : ""
        : "  --loop              treat ARG_1 and ARG_2 as the CHOICE and SHUFFLE keys,"
        : "                      entering an interactive prompt where different PUBLIC"
        : "                      keys and different options can be supplied"
        : ""
        : "  --clip              send output to the system clipboard instead of stdout."
        : "                      This functionality is supported by the standard hash"
        : "                      action, key/spell/num/mod generation actions, the info"
        : "                      action and, most importantly, the --loop flag"
        : ""
        : "  --no-clip           override the --clip option and write to stdout"
        : ""
        : "  --gen-keys          generate a random choice-shuffle keypair. The"
        : "                      key range depends on the configuration used"
        : ""
        : "  --gen-spell         prompt for an arithmetic key (e.g. `34+78^3` or `456`)"
        : "                      and print the mnemonic spell corresponding to this key"
        : ""
        : "  --gen-num           prompt for a mnemonic key (e.g. `mufasa` or `begepo`)"
        : "                      and print the numeric value of this key"
        : ""
        : "                      (both `--gen-spell` and `--gen-num` only use the first"
        : "                      argument ARG_1)"
        : ""
        : "  --gen-mod           interpret the three arguments as usual, and print"
        : "                      the CHOICE and SHUFFLE keys modulo the current source"
        : "                      configuration. For example, when combined with"
        : "                      `-k mediumpin`, the CHOICE key will be printed modulo"
        : "                      151200 (the spread of the merge-choice function), while"
        : "                      the shuffle key will be printed modulo 720 (the spread"
        : "                      of the shuffle function). The PUBLIC key may determine"
        : "                      the source configuration if configuration files are"
        : "                      enabled, i.e. if the `--impure` or `-f` options are"
        : "                      used."
        : ""
        : "  --ask-repeat        ask the user to repeat private keys, which is useful"
        : "                      when generating passwords for the first time"
        : ""
        : "  --show              do not conceal typed private keys"
        : ""
        : "  --plain             omit prompts and other decorative output when"
        : "                      appropriate"
        : ""
        : "  --pure              ignore all configuration files, the default behavior"
        : ""
        : "  --impure            enable configuration file usage"
        : ""
        : "  +color              enable colors in error messages"
        : ""
        : "  +no-color           disable colors in error messages"
        : ""
        : "                      (meta-options starting with '+' are parsed at the"
        : "                      very end of the execution chain)"
        : ""
        : "  -k KEYWORD          specify the source configuration. KEYWORD can be"
        : "                      one of the following (default is long):"
        : "                       * max (26 upper, 26 lower, 12 special, 10 digits)"
        : "                       * long (8 upper, 8 lower, 5 special, 4 digits)"
        : "                       * medium (5 symbols of each type above)"
        : "                       * short (4 symbols of each type)"
        : "                       * anlong (7 upper case, 7 lower case, 7 digits)"
        : "                       * anshort (4 upper case, 4 lower case, 4 digits)"
        : "                       * pin (4-digit pin code)"
        : "                       * mediumpin (6-digit pin code)"
        : "                       * longpin (8-digit pin code)"
        : ""
        : "  -n \"(L, U, S, D)\"   specify how many [L]ower case, [U]pper case,"
        : "                      [S]pecial characters, and [D]igits should be used"
        : ""
        : "  -c CONFIGURATION    specify the source configuration manually,"
        : "                      as the Haskell Config type"
        : ""
        : "  -i KEYWORD          show meta information. KEYWORD can be one of:"
        : "                       * help (same as `--help`)"
        : "                       * version (same as `--version`)"
        : "                       * numbers (show the total amounts of hashes/keys)"
        : "                       * times (show times needed to crack your passwords)"
        : ""
        : "                      (the output will depend on the source configuration"
        : "                      used)"
        : ""
        : "  -q KEYWORD          retrieve one of the keys from a final pseudo-hash and"
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
        : "                         PUBLIC_1, PUBLIC_2, ... PUBLIC_N : ARGS"
        : "                      (a syntax error will be reported for incorrect lines)"
        : ""
        : "                      a line with the keyword \"+all\" as PUBLIC will apply"
        : "                      to all public keys"
        : ""
        : "                      the arguments given in the configuration file are"
        : "                      superseded by those passed on the command line."
        : ""
        : "  -p SHIFT            shift all characters in the public key by the"
        : "                      specified amount. This option is generally discouraged,"
        : "                      but sometimes necessary to create multiple passwords"
        : "                      with one set of keys. When using the `-q public` option"
        : "                      in combination with `-p`, a reverse shift will be"
        : "                      applied."
        : ""
        : "  -e FILE             encrypt FILE. Accepts three arguments:"
        : "                        * WRITE TO: the file to write the encrypted/decrypted"
        : "                                    data to. A value of `stdout` will write"
        : "                                    to standard output"
        : "                        * CHOICE KEY: first encryption key"
        : "                        * SHUFFLE KEY: second encryption key"
        : ""
        : "  -d FILE             decrypt FILE. Accepts the same arguments"
        : ""
        :("  -r N                use N rounds of encryption. The default is " ++ show defaultRounds)
        : ""
        : "using source configuration:"
        : show' config : []
infoAction plain clip "version" _ =
  let text = (if plain then "" else "The pshash pseudo-hash password manager, version ") ++ currentVersion
   in outputStrLn clip text
infoAction plain clip "numbers" config =
  let amts = map dropElementInfo config
      numHashes = numberOfHashes amts
      numChoice = numberOfChoiceKeys amts
      numShuffle = numberOfShuffleKeys $ map snd amts
      numRepetitions = numberOfRepetitions $ map snd amts
      text = if plain then show numChoice ++ "\n" ++ show numShuffle ++ "\n" ++ show numRepetitions
        else
          "\n" ++
          "      symbol distribution : " ++ show amts ++ "\n" ++
          "  number of pseudo-hashes : " ++ show numHashes ++ " > " ++ printBits numHashes ++ "\n" ++
          " total pseudo-hash length : " ++ show ((sum . map snd) amts) ++ " symbols\n\n" ++
          "   number of choice  keys : " ++ show numChoice ++ " > " ++ printBits numChoice ++ "\n" ++
          "   number of shuffle keys : " ++ show numShuffle ++ " > " ++ printBits numShuffle ++ "\n" ++
          "   pseudo-hash collisions : " ++ show numRepetitions ++ " > " ++ printBits numRepetitions ++ "\n\n" ++
          "    max public key length : " ++ show (maxLengthOfPublicKey amts) ++ " symbols\n"
   in outputStrLn clip text
infoAction plain clip "times" config =
  let amts = map dropElementInfo config
      bfTime = timeToCrack (numberOfHashes amts)
      khTime = timeToCrack (numberOfRepetitions $ map snd amts)
      text = if plain then show bfTime ++ "\n" ++ show khTime
        else
          "\n" ++
          "        symbol distribution : " ++ show amts ++ "\n" ++
          "       assumed attack speed : " ++ "10 billion operations per second\n" ++
          printTimes "  password brute-force time" bfTime ++ "\n" ++
          printTimes " known password attack time" khTime ++ "\n"
   in outputStrLn clip text
infoAction _ _ cmd _ = return . Error $ [("<Info command not recognized: {{" ++ cmd ++ "}}.>") :=> []]

queryAction :: Bool -> Bool -> String -> Map OptionName String -> [Char] -> String -> String -> IO (Result ())
queryAction plain clip kwd args arg1 arg2 arg3 =
  let printPublic = if plain then outputStrLn clip . show else \s -> outputStrLn clip $ "\n public key : " ++ show s ++ "\n"
      printPrivate :: Integer -> IO (Result ())
      printPrivate = if plain then outputStrLn clip . show else \n -> outputStrLn clip $
        "\n" ++
        replicate (8 - length kwd) ' ' ++ kwd ++ " key : " ++ show n ++ "\n" ++
        " incantation : " ++ getMnemonic n ++ "\n"
      privateAction msg f = do
        mnewArgs <- addConfigArgs args (Just arg1)
        handleWithMsgM' "Adding arguments from config file:" mnewArgs $ \newArgs ->
          handleWithMsgM' "Parsing source configuration:" (getConfig newArgs) $ \config ->
          handleWithMsgM' "Applying public key patch:" (patchString newArgs False arg1) $ \newArg1 ->
          handleWithMsgM' msg (f config newArg1 arg2 arg3) printPrivate
   in case kwd of
    "public" ->
      handleWithMsgM' "Parsing source configuration:" (getConfig args) $ \config ->
      handleWithMsgM' "Retrieving public key:" (retrievePublicKey config arg1 arg2 arg3) $ \str ->
      handleWithMsgM' "Un-applying the public key patch:" (patchString args True str) printPublic
    "choice" -> privateAction "Retrieving choice key:" retrieveChoiceKey
    "shuffle" -> privateAction "Retrieving shuffle key:" retrieveShuffleKey
    _ -> return . Error $ [("<Query keyword not recognized: \"{{" ++ kwd ++ "}}\".>") :=> []]

listPairsAction :: Bool -> Config -> String -> String -> [Char] -> IO (Result ())
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
   in handleWithMsgM' "Reading number of pairs to print:" mlimit $ \limit -> 
      if plain then do
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

keygenAction :: Bool -> Bool -> Config -> IO (Result ())
keygenAction plain clip config = do
  g <- getStdGen
  let amts = map dropElementInfo config
      choice = fst $ randomR (0, numberOfChoiceKeys amts) g :: Integer
      shuffle = fst $ randomR (0, numberOfShuffleKeys $ map snd amts) g :: Integer
      text =
        if plain then show choice ++ "\n" ++ show shuffle
        else 
          "\n" ++
          "  choice key : " ++ show choice ++ "\n" ++
          " incantation : " ++ getMnemonic choice ++ "\n\n" ++
          " shuffle key : " ++ show shuffle ++ "\n" ++
          " incantation : " ++ getMnemonic shuffle ++ "\n"
  outputStrLn clip text

spellgenAction :: Map OptionName String -> IO (Result ())
spellgenAction args = do
  key <- getKeyStr args FIRST E1 P1
  handleWithMsgM' "Reading the numeric private key:" (getPrivateKeyNum key) $ \n ->
    outputStrLn (isClip args) (if member PLAIN args then getMnemonic n else "\n incantation : " ++ getMnemonic n ++ "\n")

numgenAction :: Map OptionName String -> IO (Result ())
numgenAction args = do
  mnem <- getKeyStr args FIRST E1 P1
  handleWithMsgM' "Reading the mnemonic private key:" (getPrivateKeyMnemonic mnem) $ \k ->
    outputStrLn (isClip args) (if member PLAIN args then show k else "\n numeric key : " ++ show k ++ "\n")

modgenAction :: Map OptionName String -> Config -> IO (Result ())
modgenAction args config = do
  let amts = map dropElementInfo config
  choiceStr <- getKeyStr args SECOND E2 P2
  shuffleStr <- getKeyStr args THIRD E3 P3
  liftResultM'
    (addTrace "Reading the {choice} key:" $ getPrivateKey choiceStr)
    (addTrace "Reading the {shuffle} key:" $ getPrivateKey shuffleStr) $
    \choice shuffle -> do
      let choiceSpr = numberOfChoiceKeys amts
          shuffleSpr = numberOfShuffleKeys (map snd amts)
          newChoice = mod choice choiceSpr
          newShuffle = mod shuffle shuffleSpr
          text =
            if member PLAIN args then show newChoice ++ "\n" ++ show newShuffle
            else
              "\n" ++
              " symbol distribution : " ++ show amts ++ "\n" ++
              "  choice key modulus : " ++ show choiceSpr ++ "\n" ++
              " shuffle key modulus : " ++ show shuffleSpr ++ "\n\n" ++
              "  shorter choice key : " ++ show newChoice ++ "\n" ++
              "         incantation : " ++ getMnemonic newChoice ++ "\n\n" ++
              " shorter shuffle key : " ++ show newShuffle ++ "\n" ++
              "         incantation : " ++ getMnemonic newShuffle ++ "\n\n"
      outputStrLn (isClip args) text

encryptionAction ::
  Bool ->
  Map OptionName String ->
  IO (Result ())
encryptionAction dec args = do
  let mrounds = if member ROUNDS args then readResult "integer" (args ! ROUNDS) else Content defaultRounds
      fname = args ! if dec then DECRYPT else ENCRYPT
  outfile <- getKeyStr args FIRST E1 P1
  mkey1 <- getPrivateKey <$> getKeyStr args SECOND E2 P2
  mkey2 <- getPrivateKey <$> getKeyStr args THIRD E3 P3
  mcts <- case fname of
    "stdin" -> fmap (Content . B.pack . map (fromIntegral . ord)) getContents
    _ -> readFileResult B.readFile fname
  g <- getStdGen
  let write = if outfile == "stdout" then B.putStr else B.writeFile outfile
      merge4 :: Result a -> Result b -> Result c -> Result d -> (a -> b -> c -> d -> e) -> Result e
      merge4 ma mb mc md f = ma >>= \a -> mb >>= \b -> mc >>= \c -> md >>= Content . f a b c
      curAddTrace kw = addTrace ("Reading the " ++ kw)
  handleWith write $ merge4
    (curAddTrace "{number of rounds}:" mrounds)
    (curAddTrace "{choice key}:" mkey1)
    (curAddTrace "{shuffle key}:" mkey2)
    (curAddTrace (if dec then "{ciphertext}:" else "{plaintext}:") mcts) $ \ rounds k1 k2 cts ->
      let (iv, cts') = if dec then B.splitAt (fromIntegral defaultSize) cts else (fromStrict . fst $ genByteString defaultSize g, cts)
      in (if dec then id else B.append iv) $ procrypt rounds (iv, cts') k1 k2

unprefix :: Char -> String -> String
unprefix c (c':rest)
  | c == c' = unprefix c rest
unprefix _ str = str

groupContents :: [String] -> Result (Int, [(String, String)])
groupContents [] = Content (0, [])
groupContents (('#' : _) : rest) = groupContents rest
groupContents (line : rest) = case splitBy ':' line of
  ["+all", argStr] -> Content (1, [("*", unprefix ' ' argStr)])
  [kwstr, argStr] ->
    let kws = splitBy ',' (filter (/= ' ') kwstr)
        len = foldr max 0 (map length kws)
        argStr' = unprefix ' ' argStr
        cur = map (\x -> (x, argStr')) kws
     in fmap (\ (a, lst) -> (max a len, cur ++ lst)) (groupContents rest)
  _ -> Error $ ["<Incorrect syntax:>" :=> [ ("In line {" ++ line ++ "}") :=> [] ]]

printGroups :: Bool -> Int -> [(String, String)] -> IO ()
printGroups plain len grps = do
  let formatPairPlain :: (String, String) -> String
      formatPairPlain (kw, argStr) = kw ++ ": " ++ argStr
      formatPairFancy :: (String, String) -> String
      formatPairFancy (kw, argStr) =
        " " ++ kw ++ replicate (len - length kw) ' ' ++ " | " ++ argStr
  unless plain $ putStr $
    "\n public key" ++ replicate (len - 10) ' ' ++ " | arguments\n " ++
    replicate len '-' ++ "-+" ++ replicate 10 '-' ++ "\n"
  putStr . unlines $ map (if plain then formatPairPlain else formatPairFancy) grps
  unless plain (putStrLn "")

inspectAction :: Map OptionName String -> IO (Result ())
inspectAction args
  | member CONFIGFILE args = do
    let path = args ! CONFIGFILE
    fileContentsH <- readFileResult readFile path
    handleWithMsgM ("Reading file {" ++ path ++ "}:")
      (fmap lines fileContentsH >>= groupContents)
      (\ (len, grps) -> printGroups (member PLAIN args) (max len 10) grps)
  | otherwise = do
    homeDir <- getHomeDirectory
    let processFiles :: [(FilePath, Maybe String)] -> IO (Result ())
        processFiles [] = do
          unless (member PLAIN args) $ putStr "\n no configuration files were found\n\n"
          return (Content ())
        processFiles ((_, Nothing) : rest) = processFiles rest
        processFiles ((path, Just cnts) : _) = handleWithMsgM ("Inspecting configuration file {" ++ path ++ "}:")
          (groupContents $ lines cnts)
          (\ (len, grps) -> printGroups (member PLAIN args) (max len 10) grps)
    processFiles =<< mapM (readFileMaybe readFile . replaceChar '~' homeDir) defaultConfigFiles

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
      -- exitWith (ExitFailure 1)
    Content () -> return ()

loopAction :: Map OptionName String -> IO (Result ())
loopAction args = do
  hSetBuffering stdin NoBuffering
  mkey1 <- addTrace "Reading the choice key:" . getPrivateKey <$> getKeyStr args FIRST E1 P1
  mkey2 <- addTrace "Reading the shuffle key:" . getPrivateKey <$> getKeyStr args SECOND E2 P2
  flip handleWith ((,) <$> mkey1 <*> mkey2) $ \ (choice, shuffle) -> do
    unless (member PLAIN args) $ putStrLn "\n Private keys were pre-supplied.\n Enter the public key and additional arguments to generate pseudo-hashes.\n Type \"exit\" or hit Ctrl+C to leave the loop.\n"
    let loop :: IO ()
        loop = (getInput True False "> ") >>= \input -> case input of
          "exit" -> return ()
          [] -> loop
          _ -> do
            let rawArgs = words input
                overrideArgs = parseArgs (True, True, False) rawArgs
                mnewArgs = fmap (unionWith (const id) args) $ addTrace "Parsing override arguments:" overrideArgs
            mfinalArgs <- handleWith' (\a -> addTrace "Adding arguments from config file:" <$> addConfigArgs a (DM.lookup THIRD a)) mnewArgs
            let pair = mfinalArgs >>= \finalArgs -> addTrace "Setting the source configuration:" (getConfig finalArgs) >>= \config ->
                  let public = getPublicKey <$> case DM.lookup THIRD finalArgs of
                        Nothing -> Error ["Reading the public key:" :=> ["<In loop mode, the public key must be given inline>" :=> []]]
                        Just str -> Content str
                      choiceKey = fmap2 mod ((choice +) <$> public) (chooseAndMergeSpread' config)
                   in fmap (\x -> (isClip finalArgs, getHash config x shuffle)) choiceKey
            toIO rawArgs $ handleWith' (uncurry outputStrLn) pair
            loop
    loop

hashAction :: Bool -> Config -> String -> String -> String -> IO (Result ())
hashAction clip config publicStr choiceStr shuffleStr = handleWith' (outputStrLn clip) $ getFinalHash config publicStr choiceStr shuffleStr
