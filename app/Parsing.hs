module Parsing where

import Control.Exception (IOException, catch)
import Data.Map (Map, empty, insertWith, member, (!))
import qualified Data.Map as DM
import System.Directory (getHomeDirectory)

import Error
import Algorithm
import Data.List (intercalate)

data OptionName =
    KEYWORD | SELECT | CONFIG | INFO | QUERY | PATCH | ENCRYPT | DECRYPT | ROUNDS
  | CONFIGFILE
  | PURE | IMPURE | LIST | PLAIN | SHOW | ASKREPEAT | HELP | VERSION | GENKEYS | GENSPELL | GENNUM
  | FIRST | SECOND | THIRD
  | E1 | E2 | E3 | P1 | P2 | P3
  deriving (Eq, Ord, Show)

-- ┌───────────────────────┐
-- │ READING CONFIGURATION │
-- └───────────────────────┘

defaultConfigFiles :: [String]
defaultConfigFiles =
  [
    "./pshash.conf",
    "./.pshash.conf",
    "~/.config/pshash/pshash.conf",
    "~/.pshash.conf",
    "/etc/pshash/pshash.conf",
    "C:\\pshash.conf"
  ]

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
      str -> Error $ ("<Unrecognized configuration keyword: \"{{" ++ str ++ "}}\".>") :=> []
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
  'd' -> insert' DECRYPT s <$> parseArgs trp rest
  'r' -> insert' ROUNDS s <$> parseArgs trp rest
  ch -> Error $ ("<Unsupported option: \"{{" ++ ['-',ch] ++ "}}\".>") :=> []
parseArgs _ [['-', ch]] = Error $ ("<A short option ({{-" ++ [ch] ++ "}}) requires an argument. Use {{--help}} for details.>") :=> []
parseArgs trp (('-':'-':opt) : rest) = case opt of
  "pure" -> insert' PURE [] <$> parseArgs trp rest
  "impure" -> insert' IMPURE [] <$> parseArgs trp rest
  "list" -> insert' LIST [] <$> parseArgs trp rest
  "plain" -> insert' PLAIN [] <$> parseArgs trp rest
  "ask-repeat" -> insert' ASKREPEAT [] <$> parseArgs trp rest
  "show" -> insert' SHOW [] <$> parseArgs trp rest
  "gen-keys" -> insert' GENKEYS [] <$> parseArgs trp rest
  "gen-spell" -> insert' GENSPELL [] <$> parseArgs trp rest
  "gen-num" -> insert' GENNUM [] <$> parseArgs trp rest
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
      process :: [[String]] -> Result (Map OptionName String)
      process [] = Content empty
      process ([keywords, argStr] : rest) =
        if keywords == "+all" || publicStr `elem` splitBy ',' (filter (/= ' ') keywords)
        then addTrace ("Trace while parsing options for {" ++ keywords ++ "}:") $ parseArgs (True, True, True) (words argStr)
        else process rest
      process (lst:_) = Error $ "<Cannot use configuration file: incorrect syntax:>" :=>
        [
          ("In line {" ++ intercalate ":" lst ++ "}") :=> []
        ]

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
        if member PLAIN args
        then insert' P1 "" $ insert' P2 "" $ insert' P3 "" args
        else case args ! QUERY of
            "public" -> insert' P1 "CHOICE KEY: " . insert' P2 "SHUFFLE KEY: "
            "choice" -> insert' P1 "PUBLIC KEY: " . insert' P2 "SHUFFLE KEY: "
            "shuffle" -> insert' P1 "PUBLIC KEY: " . insert' P2 "CHOICE KEY: "
            _ -> insert' P1 "INPUT 1: " . insert' P2 "INPUT 2: "
          $ insert' P3 "FINAL HASH: " args
  | member LIST args =
      insert' E1 "" $ insert' E2 "" $ insert' E3 "" $
      (if member PLAIN args then insert' P1 "" . insert' P2 "" . insert' P3 "" else insert' P1 "PUBLIC KEY: " . insert' P2 "NUMBER OF PAIRS: " . insert' P3 "FINAL HASH: ")
      args
  | member ENCRYPT args || member DECRYPT args =
      insert' E1 "" $ (if member SHOW args then insert' E2 "" . insert' E3 "" else id) $
      (if member PLAIN args then insert' P1 "" . insert' P2 "" . insert' P3 "" else insert' P1 "WRITE TO: " . insert' P2 "CHOCE KEY: " . insert' P3 "SHUFFLE KEY: ")
      args
  | member GENSPELL args =
      (if member SHOW args then insert' E1 "" . insert' E2 "" . insert' E3 "" else id) $
      ((if member PLAIN args then insert' P1 "" else insert' P1 "NUMERIC KEY: ") . insert' P2 "" . insert' P3 "")
      args
  | member GENNUM args =
      (if member SHOW args then insert' E1 "" . insert' E2 "" . insert' E3 "" else id) $
      ((if member PLAIN args then insert' P1 "" . insert' P2 "" else insert' P1 "MNEMONIC SPELL: " . insert' P2 "MODULO: ") . insert' P3 "")
      args
  | otherwise =
      insert' E1 "" $ (if member SHOW args then insert' E2 "" . insert' E3 "" else id) $
      (if member PLAIN args then insert' P1 "" . insert' P2 "" . insert' P3 "" else insert' P1 "PUBLIC KEY: " . insert' P2 "CHOICE KEY: " . insert' P3 "SHUFFLE KEY: ")
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
