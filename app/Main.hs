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
import Data.Char (ord, chr, toUpper)
import Control.Monad (unless)

import Algorithm
import Error
import Inverse
import Info

currentVersion :: String
currentVersion = "0.1.14.7"

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
    KEYWORD | SELECT | CONFIG | INFO | QUERY | PATCH
  | CONFIGFILE | CONFIGKEYWORD
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
        : "              [ -d|n|c|i|q|f|p ARGUMENT ]"
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
        : "  -d KEYWORD          Specify the source configuration. KEYWORD can be"
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
        : "main arguments:"
        : "  PUBLIC              Stands for public key, a memorable string indicative"
        : "                      of the password destination (e.g. \"google\", \"steam\")"
        : ""
        : "  CHOICE              Stands for choice private key, a large number"
        : ("                      between 0 and " ++ formatDouble (show (fromIntegral (numberOfChoiceKeys' config) :: Double)) numberOfPlaces)
        : ""
        : "  SHUFFLE             Stands for shuffle private key, a number"
        : ("                      between 0 and " ++ formatDouble (show (fromIntegral (numberOfShuffleKeys $ map snd config) :: Double)) numberOfPlaces)
        : ""
        : "using source configuration:"
        : show' config
        : []
      return (Content ())
infoAction _ "version" = putStrLn ("The pshash pseudo-hash password manager, version " ++ currentVersion) >> return (Content ())
infoAction config "numbers" =
  let amts = map dropElementInfo config
      numHashes = numberOfHashes amts
      numHashesDouble = fromIntegral numHashes :: Double
      numChoice = numberOfChoiceKeys amts
      numChoiceDouble = fromIntegral numChoice :: Double
      numShuffle = numberOfShuffleKeys $ map snd amts
      numShuffleDouble = fromIntegral numShuffle :: Double
      numRepetitions = numberOfRepetitions $ map snd amts
      numRepetitionsDouble = fromIntegral numRepetitions :: Double
   in do
  putStrLn $ "using the following configuration distribution: " ++ show amts
  putStrLn ""
  putStrLn $
    "total theoretical number of hashes:         "
      ++ formatInteger (show numHashes) ++ " ("
      ++ formatDouble (show numHashesDouble) numberOfPlaces ++ ")"
  putStrLn $
    "number of choice keys:                      "
      ++ formatInteger (show numChoice) ++ " ("
      ++ formatDouble (show numChoiceDouble) numberOfPlaces ++ ")"
  putStrLn $
    "number of shuffle keys:                     "
      ++ formatInteger (show numShuffle) ++ " ("
      ++ formatDouble (show numShuffleDouble) numberOfPlaces ++ ")"
  putStrLn $
    "number of key pairs with the same hash:     "
      ++ formatInteger (show numRepetitions) ++ " ("
      ++ formatDouble (show numRepetitionsDouble) numberOfPlaces ++ ")"
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
          ++ formatInteger (show inYInteger) ++ " ("
          ++ formatDouble (show inY) numberOfPlaces ++ ") years\n"
          ++ "                                             or "
          ++ formatInteger (show inAoUInteger) ++ " ("
          ++ formatDouble (show inAoU) numberOfPlaces
          ++ ") ages of the Universe"
  putStrLn ""
  putStrLn $
    let (inY, inAoU) = timeToCrack $ numberOfRepetitions $ map snd amts
        inAoUInteger = floor inAoU :: Integer
        inYInteger = floor inY :: Integer
     in "time to retrieve the keys based on a hash:      "
          ++ formatInteger (show inYInteger) ++ " ("
          ++ formatDouble (show inY) numberOfPlaces ++ ") years\n"
          ++ "                                             or "
          ++ formatInteger (show inAoUInteger) ++ " ("
          ++ formatDouble (show inAoU) numberOfPlaces
          ++ ") ages of the Universe"
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

safeReadWithHandler :: (Monad m) => (IOException -> IO (m String)) -> FilePath -> IO (m String)
safeReadWithHandler handler path = (return <$> readFile path) `catch` handler

readFileMaybe :: FilePath -> IO (Maybe String)
readFileMaybe = safeReadWithHandler (const $ return Nothing)

readFileResult :: FilePath -> IO (Result String)
readFileResult = safeReadWithHandler handler
  where handler e = return . Error $ "<Error reading configuration file:>" :=> [ show e :=> [] ]

getConfigFromContents :: Maybe String -> String -> IO (Result [([Char], Integer)])
getConfigFromContents keywordM contents = case keywordM of
  Nothing -> return . Error $ "<Cannot use configuration file: public key was not pre-supplied. Either:>" :=>
    [
      ("Disable configuration files by removing the " ++ "{--impure}" ++ " option, or") :=> [],
      ("Pass the " ++ "{--pure}" ++ " option for the same effect, or") :=> [],
      ("{Pass the public key inline}" ++ " as one of the arguments, or") :=> [],
      ("{Remove}" ++ " the configuration files.") :=> []
    ]
  Just publicStr -> process specs
    where
      specLines = filter (elem ':') (lines contents)
      specs = map (break' ':') specLines
      process :: [(String, String)] -> IO (Result [([Char], Integer)])
      process [] = return (Content defaultConfiguration)
      process ((publicStr', argStr) : rest) =
        if publicStr == publicStr' || publicStr' == "+all"
        then case parseArgs' (True, True, True) (words argStr) of
          Error tr -> return . Error $ ("Trace while parsing options for {" ++ publicStr' ++ "}:") :=> [tr]
          Content args -> getConfig (insertWith const PURE "" args)
        else process rest

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
  | any (`member` args) [PURE, INFO, QUERY, LIST] = return (Content defaultConfiguration)
  | member CONFIGFILE args = do
      fileContentsH <- readFileResult (args ! CONFIGFILE)
      case fileContentsH of
        Error tr -> return (Error $ ("Trace while reading contents from {" ++ args ! CONFIGFILE ++ "}:") :=> [tr])
        Content contents -> getConfigFromContents (DM.lookup CONFIGKEYWORD args) contents
  | not (member IMPURE args) = return (Content defaultConfiguration)
  | otherwise = do
      let replaceChar :: Char -> String -> String -> String
          replaceChar _ _ "" = ""
          replaceChar old new (ch : rest)
            | ch == old = new ++ replaceChar old new rest
            | otherwise = ch : replaceChar old new rest
      homeDir <- getHomeDirectory
      fileContentsM <- mapM (readFileMaybe . replaceChar '~' homeDir) defaultConfigFiles
      let findContents :: [Maybe String] -> IO (Result [([Char], Integer)])
          findContents [] = return (Content defaultConfiguration)
          findContents (Nothing : rest) = findContents rest
          findContents (Just contents : _) = getConfigFromContents (DM.lookup CONFIGKEYWORD args) contents
      findContents fileContentsM

insert' :: (Ord k) => k -> a -> Map k a -> Map k a
insert' = insertWith (const id)

parseArgs :: (Bool, Bool, Bool) -> [String] -> Result (Map OptionName String)
parseArgs _ [] = Content empty
parseArgs trp (('+':_) : rest) = parseArgs trp rest
parseArgs trp (['-', opt] : s : rest) = case opt of
  'd' -> insert' KEYWORD s <$> parseArgs trp rest
  'n' -> insert' SELECT s <$> parseArgs trp rest
  'c' -> insert' CONFIG s <$> parseArgs trp rest
  'i' -> insert' INFO s <$> parseArgs trp rest
  'q' -> insert' QUERY s <$> parseArgs trp rest
  'f' -> insert' CONFIGFILE s <$> parseArgs trp rest
  'p' -> insert' PATCH s <$> parseArgs trp rest
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

parseArgs' :: (Bool, Bool, Bool) -> [String] -> Result (Map OptionName String)
parseArgs' trp = addTrace "Trace while parsing arguments:" . raise patchArgs . parseArgs trp

patchArgs :: Map OptionName String -> Result (Map OptionName String)
patchArgs args
  | member QUERY args = Content args
  | member PATCH args =
    if member FIRST args
    then do
      patchAmount <- (readResult "integer" (args ! PATCH) :: Result Integer)
      Content
        $ insertWith const FIRST (shiftString patchAmount (args ! FIRST))
        $ insertWith const CONFIGKEYWORD (args ! FIRST) args
    else Error $ "<Cannot patch public key that was not pre-supplied. Either:>" :=>
      [
        ("{Pass the public key inline}" ++ " as one of the arguments, or") :=> [],
        ("{Remove}" ++ " the " ++ "{-p}" ++ " option.") :=> []
      ]
  | member FIRST args = Content $ insertWith const CONFIGKEYWORD (args ! FIRST) args
  | otherwise = Content args

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
  (baseStr, powStr) = break' '-' s
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
  unless (echo || os == "mingw32") $ hPutChar stderr '\n'
  if not echo && askRepeat then do
    unless (null prompt) $ hPutStr stderr ("(repeat)" ++ replicate (length prompt - 10) ' ' ++ ": ")
    inputRepeat <- withEcho echo getLine
    unless (os == "mingw32") $ hPutChar stderr '\n'
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
            _ -> insert' P1 "INPUT: " . insert' P2 "INPUT: "
          $ insert' P3 "FINAL HASH: " args
  | member LIST args =
      insert' E1 "" $ insert' E2 "" $ insert' E3 "" $
      if member NOPROMPTS args
      then insert' P1 "" $ insert' P2 "" $ insert' P3 "" args
      else insert' P1 "PUBLIC KEY: " $ insert' P2 "NUMBER OF PAIRS: " $ insert' P3 "FINAL HASH: " args
  | otherwise =
      insert' E1 "" $ (if member SHOW args then insert' E2 "" . insert' E3 "" else id) $
      if member NOPROMPTS args
      then insert' P1 "" $ insert' P2 "" $ insert' P3 "" args
      else insert' P1 "PUBLIC KEY: " $ insert' P2 "CHOICE KEY: " $ insert' P3 "SHUFFLE KEY: " args

performAction :: Map OptionName String -> Result [([Char], Integer)] -> IO (Result ())
performAction _ (Error tr) = return (Error $ "Trace in configuration argument:" :=> [tr])
performAction args (Content config)
  | member INFO args = infoAction config (args ! INFO)
  | member QUERY args = passKeysToAction args (queryAction config (args ! QUERY))
  | member LIST args = passKeysToAction args (listPairsAction config)
  | otherwise = passKeysToAction args (hashAction config)

printTraceList :: [Bool] -> [Trace] -> IO ()
printTraceList _ [] = return ()
printTraceList places [tr] = printTrace places [False,False] tr
printTraceList places (tr:rest) = printTrace places [True, False] tr >> printTraceList places rest

printTrace :: [Bool] -> [Bool] -> Trace -> IO ()
printTrace places pass (msg :=> lst) = do
  let prefix = map (\b -> if b then '|' else ' ') places ++ "|_"
  hPutStrLn stderr $ prefix ++ msg
  printTraceList (places ++ pass) lst

formatWithColor :: String -> String
formatWithColor [] = []
formatWithColor ('<':rest) = "\ESC[31m" ++ formatWithColor rest
formatWithColor ('>':rest) = "\ESC[0m" ++ formatWithColor rest
formatWithColor ('{':'{':rest) = "\ESC[33m" ++ formatWithColor rest
formatWithColor ('}':'}':rest) = "\ESC[31m" ++ formatWithColor rest
formatWithColor ('{':rest) = "\ESC[33m" ++ formatWithColor rest
formatWithColor ('}':rest) = "\ESC[0m" ++ formatWithColor rest
formatWithColor (c:rest) = c : formatWithColor rest

formatWithoutColor :: String -> Bool -> String
formatWithoutColor [] _ = []
formatWithoutColor ('<':rest) _ = formatWithoutColor rest True
formatWithoutColor ('>':rest) _ = formatWithoutColor rest False
formatWithoutColor ('{':'{':rest) _ = '*' : formatWithoutColor rest False
formatWithoutColor ('}':'}':rest) _ = '*' : formatWithoutColor rest True
formatWithoutColor ('{':rest) upper = '*' : formatWithoutColor rest upper
formatWithoutColor ('}':rest) upper = '*' : formatWithoutColor rest upper
formatWithoutColor (c:rest) True = toUpper c : formatWithoutColor rest True
formatWithoutColor (c:rest) False = c : formatWithoutColor rest False

formatTrace :: Bool -> Trace -> Trace
formatTrace True (msg :=> trs) = formatWithColor msg :=> map (formatTrace True) trs
formatTrace False (msg :=> trs) = formatWithoutColor msg False :=> map (formatTrace False) trs

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
main = getArgs >>= toIO <*> (raiseH' (raise2' performAction <*> getConfig) . fmap setEchoesAndPrompts . parseArgs' (False, False, False))
