module Parsing where

import Control.Exception (catch, SomeException)
import Data.Map (Map, empty, insertWith, member, (!))
import qualified Data.Map as DM
import System.Directory (getHomeDirectory)

import Error
import Algorithm
import Data.List (intercalate)

data OptionName =
    CONFIG | INFO | QUERY | PATCH | ENCRYPT | DECRYPT | ROUNDS
  | CONFIGFILE | INSPECT
  | PURE | IMPURE | LIST | PLAIN | SHOW | ASKREPEAT | HELP | VERSION | LOOP | CLIP | NOCLIP
  | GENKEYS | GENSPELL | GENNUM | GENMOD
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

checkConfigValidity :: Config -> Result Config
checkConfigValidity [] = Error $ ["<The empty configuration \"{{[]}}\" is considered invalid.>" :=> []]
checkConfigValidity [(lst, num)]
  | num < 0 = Error $ [("<Invalid configuration: number {{" ++ show num ++ "}} is negative.>") :=> []]
  | num > length' lst = Error $ ["<Invalid configuration: too many elements drawn.>" :=> [
        ("Using source: {" ++ show lst ++ "}") :=> [],
        ("Available amount: {" ++ show (length lst) ++ "}") :=> [],
        ("Demanded: {" ++ show num ++ "}") :=> []
      ]]
  | otherwise = Content [(lst, num)]
checkConfigValidity (src : rest) = liftA2 (++) (checkConfigValidity [src]) (checkConfigValidity rest)

safeReadWithHandler :: (Monad m) => (FilePath -> IO a) -> (SomeException -> IO (m a)) -> FilePath -> IO (m a)
safeReadWithHandler rf handler path = (return <$> rf path) `catch` handler

readFileMaybe :: (FilePath -> IO a) -> FilePath -> IO (FilePath, Maybe a)
readFileMaybe rf path = do
  mcts <- safeReadWithHandler rf (const $ return Nothing) path
  return (path, mcts)

readFileResult :: (FilePath -> IO a) -> FilePath -> IO (Result a)
readFileResult rf = safeReadWithHandler rf handler
  where handler e = return . Error $ ["<Error reading file:>" :=> [ show e :=> [] ]]

getConfig :: Map OptionName String -> Result Config
getConfig args = case DM.lookup CONFIG args of
  Just ('k':rest) -> case rest of
    "max" -> Content maxConfiguration
    "long" -> Content defaultConfiguration
    "medium" -> Content mediumConfiguration
    "short" -> Content shortConfiguration
    "anlong" -> Content anlongConfiguration
    "anshort" -> Content anshortConfiguration
    "pin" -> Content pinCodeConfiguration
    "mediumpin" -> Content mediumPinCodeConfiguration
    "longpin" -> Content longPinCodeConfiguration
    str -> Error $ [("<Unrecognized configuration keyword: \"{{" ++ str ++ "}}\".>") :=> []]
  Just ('s':rest) -> 
    readResult "(Int,Int,Int,Int)" rest
    >>= (checkConfigValidity . getConfigFromSpec)
  Just ('c':rest) -> 
    readResult "source configuration" rest
    >>= checkConfigValidity
  _ -> Content defaultConfiguration

insert' :: (Ord k) => k -> a -> Map k a -> Map k a
insert' = insertWith (const id)

parseArgs :: (Bool, Bool, Bool) -> [String] -> Result (Map OptionName String)
parseArgs _ [] = Content empty
parseArgs trp (('+':_) : rest) = parseArgs trp rest
parseArgs trp (['-', opt] : s : rest) = case opt of
  'k' -> insert' CONFIG ('k':s) <$> parseArgs trp rest
  'n' -> insert' CONFIG ('s':s) <$> parseArgs trp rest
  'c' -> insert' CONFIG ('c':s) <$> parseArgs trp rest
  'i' -> insert' INFO s <$> parseArgs trp rest
  'q' -> insert' QUERY s <$> parseArgs trp rest
  'f' -> insert' CONFIGFILE s <$> parseArgs trp rest
  'p' -> insert' PATCH s <$> parseArgs trp rest
  'e' -> insert' ENCRYPT s <$> parseArgs trp rest
  'd' -> insert' DECRYPT s <$> parseArgs trp rest
  'r' -> insert' ROUNDS s <$> parseArgs trp rest
  ch -> Error $ [("<Unsupported short option: \"{{" ++ ['-',ch] ++ "}}\".>") :=> []]
parseArgs _ [['-', ch]] = Error $ [("<A short option ({{-" ++ [ch] ++ "}}) requires an argument. Use {{--help}} for details.>") :=> []]
parseArgs trp (('-':'-':opt) : rest) = case opt of
  "pure" -> insert' PURE [] <$> parseArgs trp rest
  "impure" -> insert' IMPURE [] <$> parseArgs trp rest
  "inspect" -> insert' INSPECT [] <$> parseArgs trp rest
  "list" -> insert' LIST [] <$> parseArgs trp rest
  "plain" -> insert' PLAIN [] <$> parseArgs trp rest
  "ask-repeat" -> insert' ASKREPEAT [] <$> parseArgs trp rest
  "show" -> insert' SHOW [] <$> parseArgs trp rest
  "gen-keys" -> insert' GENKEYS [] <$> parseArgs trp rest
  "gen-spell" -> insert' GENSPELL [] <$> parseArgs trp rest
  "gen-num" -> insert' GENNUM [] <$> parseArgs trp rest
  "gen-mod" -> insert' GENMOD [] <$> parseArgs trp rest
  "loop" -> insert' LOOP [] <$> parseArgs trp rest
  "clip" -> insert' CLIP [] <$> parseArgs trp rest
  "no-clip" -> insert' NOCLIP [] <$> parseArgs trp rest
  "help" -> insert' INFO "help" <$> parseArgs trp rest
  "version" -> insert' INFO "version" <$> parseArgs trp rest
  str -> Error $ [("<Unsupported long option: {{--" ++ str ++ "}}.>") :=> []]
parseArgs _ (['-'] : _) = Error $ ["<All dashes should be followed by command line options.>" :=> []]
parseArgs _ (('-':ch:opt) : _) = Error $ ["<Violation of command line option format. Try:>" :=> [
    ("{" ++ ('-':'-':ch:opt) ++ "} for long option, or") :=> [],
    ("{" ++ ['-',ch] ++ "} for short option.") :=> []
  ]]
parseArgs (b1, b2, b3) (s : rest)
  | b3 = Error $ [("<Excessive argument: {{" ++ s ++ "}}. All three were already provided.>") :=> []]
  | b2 = insert' THIRD s <$> parseArgs (True, True, True) rest
  | b1 = insert' SECOND s <$> parseArgs (True, True, False) rest
  | otherwise = insert' FIRST s <$> parseArgs (True, False, False) rest

isClip :: Map OptionName String -> Bool
isClip args = member CLIP args && not (member NOCLIP args)

getArgsFromContents :: String -> String -> Result (Map OptionName String)
getArgsFromContents pubstr contents = findArgs $ map (splitBy ':') (lines contents)
  where
    findArgs :: [[String]] -> Result (Map OptionName String)
    findArgs [] = Content empty
    findArgs ((('#' : _) : _) : rest) = findArgs rest
    findArgs ([keywords, argStr] : rest) =
      let lst = splitBy ',' (filter (/= ' ') keywords) in
      if "+all" `elem` lst || pubstr `elem` lst
      then addTrace ("Parsing options for public key {" ++ pubstr ++ "}:") $ parseArgs (True, True, True) (words argStr)
      else findArgs rest
    findArgs (lst : _) = Error $ ["<Incorrect syntax:>" :=> [
        ("In line {" ++ intercalate ":" lst ++ "}") :=> []
      ]]

replaceChar :: Char -> String -> String -> String
replaceChar _ _ "" = ""
replaceChar old new (ch : rest)
  | ch == old = new ++ replaceChar old new rest
  | otherwise = ch : replaceChar old new rest

getConfigArgs :: Map OptionName String -> Maybe String -> IO (Result (Map OptionName String))
getConfigArgs args Nothing = return (Content args)
getConfigArgs args (Just pubstr)
  | member PURE args = return (Content args)
  | member CONFIGFILE args = do
      let path = args ! CONFIGFILE
      mcts <- readFileResult readFile path
      return $ addTrace ("Reading settings from {" ++ path ++ "}:") $ mcts >>= getArgsFromContents pubstr
  | not (member IMPURE args) = return (Content args)
  | otherwise = do
      let processContents :: [(FilePath, Maybe String)] -> Result (Map OptionName String)
          processContents ((path, Just cts) : _) = addTrace ("Reading settings from {" ++ path ++ "}:") $ getArgsFromContents pubstr cts
          processContents ((_, Nothing) : rest) = processContents rest
          processContents [] = Content args
      homeDir <- getHomeDirectory
      return . processContents =<< mapM (readFileMaybe readFile . replaceChar '~' homeDir) defaultConfigFiles

patchString :: Map OptionName String -> Bool -> String -> Result String
patchString args inv str
  | member PATCH args = do
      patchAmount <- (readResult "integer" (args ! PATCH) :: Result Integer)
      Content $ shiftString (if inv then -patchAmount else patchAmount) str
  | otherwise = Content str

setEchoesAndPrompts :: Map OptionName String -> Map OptionName String
setEchoesAndPrompts args
  | member INFO args || member GENKEYS args = args
  | member QUERY args =
      (if member SHOW args then insert' E1 "" . insert' E2 "" . insert' E3 "" else id) $
        if member PLAIN args
        then insert' P1 "" $ insert' P2 "" $ insert' P3 "" args
        else case args ! QUERY of
            "public" -> insert' P1 "CHOICE KEY: " . insert' P2 "SHUFFLE KEY: "
            "choice" -> insert' P1 "PUBLIC KEY: " . insert' P2 "SHUFFLE KEY: "
            "shuffle" -> insert' P1 "PUBLIC KEY: " . insert' P2 "CHOICE KEY: "
            _ -> insert' P1 "INPUT 1: " . insert' P2 "INPUT 2: "
          $ insert' P3 "FINAL HASH: " args
  | member LIST args =
      (insert' E1 "" . insert' E2 "" . if member SHOW args then insert' E3 "" else id) $
      (if member PLAIN args then insert' P1 "" . insert' P2 "" . insert' P3 "" else insert' P1 "PUBLIC KEY: " . insert' P2 "NUMBER OF PAIRS: " . insert' P3 "FINAL HASH: ")
      args
  | member ENCRYPT args || member DECRYPT args =
      insert' E1 "" $ (if member SHOW args then insert' E2 "" . insert' E3 "" else id) $
      (if member PLAIN args then insert' P1 "" . insert' P2 "" . insert' P3 "" else insert' P1 "WRITE TO: " . insert' P2 "CHOICE KEY: " . insert' P3 "SHUFFLE KEY: ")
      args
  | member LOOP args =
      (if member SHOW args then insert' E1 "" . insert' E2 "" . insert' E3 "" else id) $
      (if member PLAIN args then insert' P1 "" . insert' P2 "" . insert' P3 "" else insert' P1 "CHOICE KEY: " . insert' P2 "SHUFFLE KEY: " . insert' P3 "PUBLIC KEY: ")
      args
  | member GENSPELL args =
      (if member SHOW args then insert' E1 "" else id) $
      (if member PLAIN args then insert' P1 "" else insert' P1 "NUMERIC KEY: ")
      args
  | member GENNUM args =
      (if member SHOW args then insert' E1 "" else id) $
      (if member PLAIN args then insert' P1 "" else insert' P1 "MNEMONIC SPELL: ")
      args
  | member GENMOD args =
      (if member SHOW args then insert' E1 "" . insert' E2 "" . insert' E3 "" else id) $
      (if member PLAIN args then insert' P1 "" . insert' P2 "" . insert' P3 "" else insert' P1 "PUBLIC KEY: " . insert' P2 "CHOICE KEY: " . insert' P3 "SHUFFLE KEY: ")
      args
  | otherwise =
      insert' E1 "" $ (if member SHOW args then insert' E2 "" . insert' E3 "" else id) $
      (if member PLAIN args then insert' P1 "" . insert' P2 "" . insert' P3 "" else insert' P1 "PUBLIC KEY: " . insert' P2 "CHOICE KEY: " . insert' P3 "SHUFFLE KEY: ")
      args

addConfigArgs :: Map OptionName String -> Maybe String -> IO (Result (Map OptionName String))
addConfigArgs args mpub = fmap (fmap $ DM.union args) (getConfigArgs args mpub)
