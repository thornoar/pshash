{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# HLINT ignore "Use list literal" #-}
module Main where

import System.IO (stdin, stderr, hGetEcho, hSetEcho, hPutStr, hPutChar, hPutStrLn)
import Data.Map (Map, empty, insertWith, member, (!))
import qualified Data.Map as DM
import System.Environment (getArgs)
import System.Directory (getHomeDirectory)
import System.Info (os)
import Control.Applicative (liftA2)
import Control.Exception (IOException, catch, bracket_)
import System.Exit (exitWith, ExitCode (ExitFailure))
import Data.Char (ord, chr)
import Control.Monad (unless)
import qualified Data.ByteString as BS (readFile, unpack, writeFile, putStr, pack, ByteString)

import Algorithm
import Error
import Inverse
import Info
import Encryption
import Data.Word (Word8)

currentVersion :: String
currentVersion = "0.1.16.5"

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

-- ┌─────────────────┐
-- │ QUERY FUNCTIONS │
-- └─────────────────┘

checkConfigValidity :: [([Char], Integer)] -> Result [([Char], Integer)]
checkConfigValidity [] = Error $ "<Cannot have empty configuration.>" :=> []
checkConfigValidity [(lst, num)]
  | num < 0 = Error $ ("<Invalid configuration: number {{" ++ show num ++ "}} must be non-negative.>") :=> []
  | num > length' lst = Error $ "<Invalid configuration: too many elements drawn.>" :=>
      [
        ("Using source: {" ++ show lst ++ "}") :=> [],
        ("Available amount: {" ++ show (length lst) ++ "}") :=> [],
        ("Demanded: {" ++ show num ++ "}") :=> []
      ]
  | otherwise = Content [(lst, num)]
checkConfigValidity (src : rest) = case checkConfigValidity [src] of
  Error tr -> Error tr
  Content src' -> (src' ++) <$> checkConfigValidity rest

getConfigFromSpec :: (Integer, Integer, Integer, Integer) -> [([Char], Integer)]
getConfigFromSpec (a,b,c,d) = [(sourceLower, a), (sourceUpper, b), (sourceSpecial, c), (sourceNumbers, d)]

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

-- ┌────────────────┐
-- │ USER INTERFACE │
-- └────────────────┘

data OptionName =
    KEYWORD | SELECT | CONFIG | INFO | QUERY | PATCH | ENCRYPT
  | CONFIGFILE
  | PURE | IMPURE | LIST | NOPROMPTS | SHOW | ASKREPEAT | HELP | VERSION
  | FIRST | SECOND | THIRD
  | E1 | E2 | E3 | P1 | P2 | P3
  deriving (Eq, Ord, Show)

defaultConfigFiles :: [String]
defaultConfigFiles =
  [
    "./pshash.conf",
    "~/.config/pshash/pshash.conf",
    "~/.pshash.conf",
    "/etc/pshash/pshash.conf",
    "C:\\pshash.conf"
  ]

handleWith :: (a -> IO ()) -> Result a -> IO (Result ())
handleWith f ma = case ma of
  Error tr -> return (Error tr)
  Content a -> f a >> return (Content ())

infoAction :: [([Char], Integer)] -> String -> IO (Result ())
infoAction config "help" = do
      let show' :: [([Char],Integer)] -> String
          show' config' = "[\n" ++ concatMap (("  " ++) . (++ "\n") . show) config' ++ "]"
      putStrLn . unlines $
          "usage: pshash [ --help | --version | --list | --pure | --impure ]"
        : "              [ +color | +no-color ]"
        : "              [ -k|n|c|i|q|f|p|e|d|r ARGUMENT ]"
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
        : "                         PUBLIC: ARGS"
        : "                      (other lines will be ignored)"
        : "                      A line with the keyword \"+all\" as PUBLIC will apply"
        : "                      to all public keys."
        : "                      When using configuration files, the PUBLIC key needs"
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
        : "                                    data to. A value of `stdout` will write to"
        : "                                    stdout."
        : "                        * KEY 1: first encryption key (e.g. choice key)."
        : "                        * KEY 2: second encryption key (e.g. shuffle key)."
        : "                      The encryption and decryption algorithms are the same."
        : ""
        : "main arguments:"
        : "  PUBLIC              Stands for public key, a memorable string indicative"
        : "                      of the password destination (e.g. \"google\", \"steam\")"
        : ""
        : "  CHOICE              Stands for choice private key, a large number"
        : ("                      between 0 and " ++ formatNumber (numberOfChoiceKeys' config) numberOfPlaces)
        : ""
        : "  SHUFFLE             Stands for shuffle private key, a number"
        : ("                      between 0 and " ++ formatNumber (numberOfShuffleKeys $ map snd config) numberOfPlaces)
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
  putStrLn $
    "total theoretical number of hashes:         " ++
      formatNumber numHashes numberOfPlaces
  putStrLn $
    "number of choice keys:                      " ++
    formatNumber numChoice numberOfPlaces
  putStrLn $
    "number of shuffle keys:                     " ++
    formatNumber numShuffle numberOfPlaces
  putStrLn $
    "number of key pairs with the same hash:     " ++
    formatNumber numRepetitions numberOfPlaces
  putStrLn $ "total hash length:                          " ++ show ((sum . map snd) amts) ++ " symbols"
  putStrLn $ "maximum relevant length of the public key:  " ++ show (maxLengthOfPublicKey amts) ++ " symbols"
  return (Content ())
infoAction config "times" = let amts = map dropElementInfo config in do
  putStrLn $ "using the following configuration distribution: " ++ show amts
  putStrLn $ "assumed number of password checks per second:   " ++ "10 billion = 10^10"
  putStrLn $ "time to check one password:                     " ++ "10^(-10) s = 0.1 nanosecond"
  putStrLn ""
  putStrLn $
    let (inY, inAoU) = timeToCrack $ numberOfHashes amts
        inAoUInteger = floor inAoU :: Integer
        inYInteger = floor inY :: Integer
     in "time to brute-force your password:              "
          ++ formatNumber inYInteger numberOfPlaces
          ++ " years"
          ++ if inAoUInteger > 0
             then "\n"
                  ++ "                                             or "
                  ++ formatNumber inAoUInteger numberOfPlaces
                  ++ " ages of the Universe"
             else ""
  putStrLn ""
  putStrLn $
    let (inY, inAoU) = timeToCrack $ numberOfRepetitions $ map snd amts
        inAoUInteger = floor inAoU :: Integer
        inYInteger = floor inY :: Integer
     in "time to retrieve the keys based on a hash:      "
          ++ formatNumber inYInteger numberOfPlaces
          ++ " years"
          ++ if inAoUInteger > 0
             then "\n"
                  ++ "                                             or "
                  ++ formatNumber inAoUInteger numberOfPlaces
                  ++ " ages of the Universe"
             else ""
  return (Content ())
infoAction _ cmd = return . Error $ ("Info command not recognized: " ++ cmd ++ ".") :=> []

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

hashAction :: [([Char], Integer)] -> String -> String -> String -> IO (Result ())
hashAction config publicStr choiceStr shuffleStr = handleWith putStrLn $ getFinalHash config publicStr choiceStr shuffleStr

encryptionWrapper ::
  Map OptionName String ->
  (BS.ByteString -> Integer -> BS.ByteString) ->
  -- (BS.ByteString -> Integer -> Integer -> BS.ByteString) ->
  FilePath ->
  IO (Result ())
encryptionWrapper args func fname = do
  -- let mrounds = if member ROUNDS args then readResult "integer" (args ! ROUNDS) else Content defaultRounds
  outfile <- getKeyStr args FIRST E1 P1
  mkey1 <- getPrivateKey <$> getKeyStr args SECOND E2 P2
  mkey2 <- getPrivateKey <$> getKeyStr args THIRD E3 P3
  mcts <- case fname of
    "stdin" -> fmap (Content . BS.pack . map (fromIntegral . ord)) getContents
    _ -> readFileResult BS.readFile fname
  let write = if outfile == "stdout" then BS.putStr else BS.writeFile outfile
  handleWith write $ case (mkey1, mkey2, mcts) of
    (Error tr, _, _) -> Error $ (pref ++ "{first key}:") :=> [tr]
    (_, Error tr, _) -> Error $ (pref ++ "{second key}:") :=> [tr]
    (_, _, Error tr) -> Error $ (pref ++ "{plaintext}:") :=> [tr]
    (Content k1, Content k2, Content msg) -> Content $ func msg (k1 + k2)
    where pref = "Trace while performing encryption/decryption, reading the "

safeReadWithHandler :: (Monad m) => (FilePath -> IO a) -> (IOException -> IO (m a)) -> FilePath -> IO (m a)
safeReadWithHandler rf handler path = (return <$> rf path) `catch` handler

readFileMaybe :: (FilePath -> IO a) -> FilePath -> IO (Maybe a)
readFileMaybe rf = safeReadWithHandler rf (const $ return Nothing)

readFileResult :: (FilePath -> IO a) -> FilePath -> IO (Result a)
readFileResult rf = safeReadWithHandler rf handler
  where handler e = return . Error $ "<Error reading file:>" :=> [ show e :=> [] ]

getConfig :: Map OptionName String -> IO (Result [([Char], Integer)])
getConfig args
  | member KEYWORD args = return $ case args ! KEYWORD of
      "long" -> Content defaultConfiguration
      "medium" -> Content mediumConfiguration
      "short" -> Content shortConfiguration
      "anlong" -> Content anlongConfiguration
      "anshort" -> Content anshortConfiguration
      "pin" -> Content pinCodeConfiguration
      "mediumpin" -> Content mediumPinCodeConfiguration
      "longpin" -> Content longPinCodeConfiguration
      str -> Error $ ("<Unrecognized configuration keyword: {{" ++ str ++ "}}.>") :=> []
  | member SELECT args =
      return $ readResult "(Int,Int,Int,Int)" (args ! SELECT)
      >>= (checkConfigValidity . getConfigFromSpec)
  | member CONFIG args =
      return $ readResult "source configuration" (args ! CONFIG)
      >>= checkConfigValidity
  | otherwise = return (Content defaultConfiguration)

insert' :: (Ord k) => k -> a -> Map k a -> Map k a
insert' = insertWith (const id)

parseArgs :: (Bool, Bool, Bool) -> [String] -> Result (Map OptionName String)
parseArgs _ [] = Content empty
parseArgs trp (('+':_) : rest) = parseArgs trp rest
parseArgs trp (['-', opt] : s : rest) = case opt of
  'k' -> insert' KEYWORD s <$> parseArgs trp rest
  'n' -> insert' SELECT s <$> parseArgs trp rest
  'c' -> insert' CONFIG s <$> parseArgs trp rest
  'i' -> insert' INFO s <$> parseArgs trp rest
  'q' -> insert' QUERY s <$> parseArgs trp rest
  'f' -> insert' CONFIGFILE s <$> parseArgs trp rest
  'p' -> insert' PATCH s <$> parseArgs trp rest
  'e' -> insert' ENCRYPT s <$> parseArgs trp rest
  ch -> Error $ ("<Unsupported option: {{" ++ ['-',ch] ++ "}}.>") :=> []
parseArgs _ [['-', ch]] = Error $ ("<A short option ({{-" ++ [ch] ++ "}}) requires an argument. Use {{--help}} for details.>") :=> []
parseArgs trp (('-':'-':opt) : rest) = case opt of
  "pure" -> insert' PURE [] <$> parseArgs trp rest
  "impure" -> insert' IMPURE [] <$> parseArgs trp rest
  "list" -> insert' LIST [] <$> parseArgs trp rest
  "no-prompts" -> insert' NOPROMPTS [] <$> parseArgs trp rest
  "ask-repeat" -> insert' ASKREPEAT [] <$> parseArgs trp rest
  "show" -> insert' SHOW [] <$> parseArgs trp rest
  "help" -> insert' INFO "help" <$> parseArgs trp rest
  "version" -> insert' INFO "version" <$> parseArgs trp rest
  str -> Error $ ("<Unsupported option: {{--" ++ str ++ "}}.>") :=> []
parseArgs _ (['-'] : _) = Error $ "<All dashes should be followed by command line options.>" :=> []
parseArgs _ (('-':ch:opt) : _) = Error $ "<Violation of command line option format. Try:>" :=>
  [
    ("{" ++ ('-':'-':ch:opt) ++ "} for long option, or") :=> [],
    ("{" ++ ['-',ch] ++ "} for short option.") :=> []
  ]
parseArgs (b1, b2, b3) (s : rest)
  | b3 = Error $ ("<Excessive argument: {{" ++ s ++ "}}. All three keys were already provided.>") :=> []
  | b2 = insert' THIRD s <$> parseArgs (True, True, True) rest
  | b1 = insert' SECOND s <$> parseArgs (True, True, False) rest
  | otherwise = insert' FIRST s <$> parseArgs (True, False, False) rest

getArgsFromContents :: Maybe String -> String -> Result (Map OptionName String)
getArgsFromContents keywordM contents = case keywordM of
  Nothing -> Error $ "<Cannot use configuration file: public key was not pre-supplied. Either:>" :=>
    [
      ("Disable configuration files by removing the " ++ "{--impure}" ++ " option, or") :=> [],
      ("Pass the " ++ "{--pure}" ++ " option for the same effect, or") :=> [],
      ("{Pass the public key inline}" ++ " as one of the arguments, or") :=> [],
      ("{Remove}" ++ " the configuration files.") :=> []
    ]
  Just publicStr -> process $ map (splitBy ':') (filter (elem ':') (lines contents))
    where
      split :: Char -> String -> [String]
      split _ [] = []
      split c str = let (f, s) = splitBy c str in f : split c s
      process :: [(String, String)] -> Result (Map OptionName String)
      process [] = Content empty
      process ((keywords, argStr) : rest) =
        if keywords == "+all" || publicStr `elem` split ',' (filter (/= ' ') keywords)
        then addTrace ("Trace while parsing options for {" ++ keywords ++ "}:") $ parseArgs (True, True, True) (words argStr)
        else process rest

getConfigArgs :: Map OptionName String -> IO (Result (Map OptionName String))
getConfigArgs args
  | any (`member` args) [PURE, INFO, QUERY, LIST] = return (Content args)
  | member CONFIGFILE args = do
      fileContentsH <- readFileResult readFile (args ! CONFIGFILE)
      case fileContentsH of
        Error tr -> return (Error $ ("Trace while reading contents from {" ++ args ! CONFIGFILE ++ "}:") :=> [tr])
        Content contents -> return $ getArgsFromContents (DM.lookup FIRST args) contents
  | not (member IMPURE args) = return (Content args)
  | otherwise = do
      let replaceChar :: Char -> String -> String -> String
          replaceChar _ _ "" = ""
          replaceChar old new (ch : rest)
            | ch == old = new ++ replaceChar old new rest
            | otherwise = ch : replaceChar old new rest
      homeDir <- getHomeDirectory
      fileContentsM <- mapM (readFileMaybe readFile . replaceChar '~' homeDir) defaultConfigFiles
      let findContents :: [Maybe String] -> Result (Map OptionName String)
          findContents [] = Content args
          findContents (Nothing : rest) = findContents rest
          findContents (Just contents : _) = getArgsFromContents (DM.lookup FIRST args) contents
      return $ findContents fileContentsM

patchArgs :: Map OptionName String -> Result (Map OptionName String)
patchArgs args
  | member QUERY args = Content args
  | member PATCH args =
    if member FIRST args
    then do
      patchAmount <- (readResult "integer" (args ! PATCH) :: Result Integer)
      Content $ insertWith const FIRST (shiftString patchAmount (args ! FIRST)) args
    else Error $ "<Cannot patch public key that was not pre-supplied. Either:>" :=>
      [
        ("{Pass the public key inline}" ++ " as one of the arguments, or") :=> [],
        ("{Remove}" ++ " the " ++ "{-p}" ++ " option.") :=> []
      ]
  | member FIRST args = Content $ insertWith const FIRST (args ! FIRST) args
  | otherwise = Content args

setEchoesAndPrompts :: Map OptionName String -> Map OptionName String
setEchoesAndPrompts args
  | member INFO args = args
  | member QUERY args =
      insert' E1 "" $ insert' E2 "" $ insert' E3 "" $
        if member NOPROMPTS args
        then insert' P1 "" $ insert' P2 "" $ insert' P3 "" args
        else case args ! QUERY of
            "public" -> insert' P1 "CHOICE KEY: " . insert' P2 "SHUFFLE KEY: "
            "choice" -> insert' P1 "PUBLIC KEY: " . insert' P2 "SHUFFLE KEY: "
            "shuffle" -> insert' P1 "PUBLIC KEY: " . insert' P2 "CHOICE KEY: "
            _ -> insert' P1 "INPUT 1: " . insert' P2 "INPUT 2: "
          $ insert' P3 "FINAL HASH: " args
  | member LIST args =
      insert' E1 "" $ insert' E2 "" $ insert' E3 "" $
      (if member NOPROMPTS args then insert' P1 "" . insert' P2 "" . insert' P3 "" else insert' P1 "PUBLIC KEY: " . insert' P2 "NUMBER OF PAIRS: " . insert' P3 "FINAL HASH: ")
      args
  | member ENCRYPT args =
      insert' E1 "" $ (if member SHOW args then insert' E2 "" . insert' E3 "" else id) $
      (if member NOPROMPTS args then insert' P1 "" . insert' P2 "" . insert' P3 "" else insert' P1 "WRITE TO: " . insert' P2 "KEY 1: " . insert' P3 "KEY 2: ")
      args
  | otherwise =
      insert' E1 "" $ (if member SHOW args then insert' E2 "" . insert' E3 "" else id) $
      (if member NOPROMPTS args then insert' P1 "" . insert' P2 "" . insert' P3 "" else insert' P1 "PUBLIC KEY: " . insert' P2 "CHOICE KEY: " . insert' P3 "SHUFFLE KEY: ")
      args

parseArgs' :: (Bool, Bool, Bool) -> [String] -> IO (Result (Map OptionName String))
parseArgs' trp rawArgs = case parseArgs trp rawArgs of
  Error tr -> return . Error $ "Trace while parsing command-line arguments:" :=> [tr]
  Content args -> do
    configArgs <- getConfigArgs args
    return $
      addTrace "Trace while parsing configuration file arguments:" .
      fmap setEchoesAndPrompts $
      (configArgs >>= patchArgs . DM.union args)

splitBy :: (Eq a) => a -> [a] -> ([a], [a])
splitBy _ [] = ([],[])
splitBy a (a':rest)
  | a == a' = ([], rest)
  | otherwise = let (res1, res2) = splitBy a rest in (a' : res1, res2)

getPublicKey' :: String -> Integer
getPublicKey' "" = 0
getPublicKey' (c:cs) = toInteger (ord c) + 128 * getPublicKey' cs

getPublicKey :: String -> Integer
getPublicKey = getPublicKey' . reverse

getPublicStr' :: Integer -> String
getPublicStr' 0 = ""
getPublicStr' key = chr (fromInteger (mod key 128)) : getPublicStr' (div key 128)

getPublicStr :: Integer -> String
getPublicStr = reverse . getPublicStr'

getPrivateKey :: String -> Result Integer
getPrivateKey s = liftA2 (^) base pow where
  (baseStr, powStr) = splitBy '-' s
  base :: Result Integer
  base = readResult "base in private key" baseStr
  pow :: Result Integer
  pow = if null powStr then Content 1 else case readResult "exponent in private key" powStr of
    Error tr -> Error tr
    Content n ->
      if n < 0
      then Error $ ("<Cannot have negative exponent in private key: {{" ++ powStr ++ "}}.>") :=> []
      else Content n

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

passKeysToAction ::
  Map OptionName String ->
  (String -> String -> String -> IO (Result ())) ->
  IO (Result ())
passKeysToAction args act = do
  first <- getKeyStr args FIRST E1 P1
  second <- getKeyStr args SECOND E2 P2
  third <- getKeyStr args THIRD E3 P3
  act first second third

removeCodesInString :: Bool -> String -> String
removeCodesInString _ [] = []
removeCodesInString False ('m':rest) = removeCodesInString True rest
removeCodesInString False (_:rest) = removeCodesInString False rest
removeCodesInString True ('\ESC':rest) = removeCodesInString False rest
removeCodesInString True (c:rest) = c : removeCodesInString True rest

removeCodes :: Trace -> Trace
removeCodes (msg :=> trs) = removeCodesInString True msg :=> map removeCodes trs

performAction :: Map OptionName String -> Result [([Char], Integer)] -> IO (Result ())
performAction _ (Error tr) = return (Error $ "Trace in configuration argument:" :=> [tr])
performAction args (Content config)
  | member INFO args = infoAction config (args ! INFO)
  | member QUERY args = passKeysToAction args (queryAction config (args ! QUERY))
  | member LIST args = passKeysToAction args (listPairsAction config)
  | member ENCRYPT args = encryptionWrapper args procrypt (args ! ENCRYPT)
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
  toIO rawArgs $ raiseH' (raise2' performAction <*> getConfig) parsedArgs
