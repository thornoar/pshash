module Main where

import Data.Char (ord)
import Data.Map (Map, empty, insert, member, (!))
import System.Environment (getArgs)

currentVersion :: String
currentVersion = "0.1.2.0"

-- ┌───────────────────────────┐
-- │ GENERAL-PURPOSE FUNCTIONS │
-- └───────────────────────────┘

factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)

factorial' :: Integer -> Integer -> Integer
factorial' n 1 = n
factorial' n m = (n - (m - 1)) * factorial' n (m - 1)

cnk :: Integer -> Integer -> Integer
cnk n k = div (factorial' n k) (factorial k)

len :: [a] -> Integer
len = toInteger . length

dropElementInfo :: ([a], Integer) -> (Integer, Integer)
dropElementInfo (src, m) = (len src, m)

class Shifting a where
  shift :: a -> Integer

instance (Shifting a) => Shifting [a] where
  shift = sum . map shift

instance Shifting Char where
  shift = toInteger . ord

mapHashing :: (Shifting b) => (a -> Integer -> b) -> (a -> Integer) -> ([a] -> Integer -> [b])
mapHashing _ _ [] _ = []
mapHashing f spr (a : as) key = b : mapHashing f spr as nextKey
  where
    (keyDiv, keyMod) = divMod key $ spr a
    b = f a keyMod
    nextKey = keyDiv + shift b

composeHashing :: (a -> Integer -> b) -> (b -> Integer -> c) -> (a -> Integer -> Integer -> c)
composeHashing f g a key1 = g (f a key1)

composeHashing' :: (Shifting b) => (a -> Integer -> b) -> (a -> Integer) -> (b -> Integer -> c) -> (a -> Integer -> c)
composeHashing' f spr g a key = g b nextKey
  where
    (keyDiv, keyMod) = divMod key $ spr a
    b = f a keyMod
    nextKey = keyDiv + shift b

-- ┌─────────────────────────────────────────────────────┐
-- │ PRE-DEFINED STRINGS FROM WHICH HASHES WILL BE DRAWN │
-- └─────────────────────────────────────────────────────┘

sourceLower :: [Char]
sourceLower = "ckapzfitqdxnwehrolmbyvsujg"

sourceUpper :: [Char]
sourceUpper = "RQLIANBKJYVWPTEMCZSFDOGUHX"

sourceSpecial :: [Char]
sourceSpecial = "=!*@?$%#&-+^"

sourceNumbers :: [Char]
sourceNumbers = "1952074386"

defaultConfiguration :: [([Char], Integer)]
defaultConfiguration = [(sourceLower, 8), (sourceUpper, 8), (sourceSpecial, 5), (sourceNumbers, 4)]

mediumConfiguration :: [([Char], Integer)]
mediumConfiguration = [(sourceLower, 5), (sourceUpper, 5), (sourceSpecial, 5), (sourceNumbers, 5)]

shortConfiguration :: [([Char], Integer)]
shortConfiguration = [(sourceLower, 4), (sourceUpper, 4), (sourceSpecial, 4), (sourceNumbers, 4)]

anlongConfiguration :: [([Char], Integer)]
anlongConfiguration = [(sourceLower, 7), (sourceUpper, 7), (sourceNumbers, 7)]

anshortConfiguration :: [([Char], Integer)]
anshortConfiguration = [(sourceLower, 4), (sourceUpper, 4), (sourceNumbers, 4)]

pinCodeConfiguration :: [([Char], Integer)]
pinCodeConfiguration = [(sourceNumbers, 4)]

mediumPinCodeConfiguration :: [([Char], Integer)]
mediumPinCodeConfiguration = [(sourceNumbers, 6)]

longPinCodeConfiguration :: [([Char], Integer)]
longPinCodeConfiguration = [(sourceNumbers, 8)]

-- ┌───────────────────────────┐
-- │ HASH GENERATING FUNCTIONS │
-- └───────────────────────────┘

chooseOrdered :: (Eq a, Shifting a) => ([a], Integer) -> Integer -> [a]
chooseOrdered (_, 0) _ = []
chooseOrdered ([], _) _ = []
chooseOrdered (src, m) key = curElt : chooseOrdered (filter (/= curElt) src, m - 1) nextKey
  where
    (keyDiv, keyMod) = divMod key $ len src
    curElt = src !! fromIntegral keyMod
    nextKey = keyDiv + shift curElt

chooseSpread :: (Integer, Integer) -> Integer
chooseSpread (n, m) = factorial' n m

mergeTwoLists :: (Shifting a) => [a] -> [a] -> Integer -> [a]
mergeTwoLists [] lst2 _ = lst2
mergeTwoLists lst1 [] _ = lst1
mergeTwoLists lst1 lst2 key
  | curKey < spr1 =
      let elt = head lst1
       in elt : mergeTwoLists (tail lst1) lst2 (curKey + shift elt)
  | otherwise =
      let elt = head lst2
       in elt : mergeTwoLists lst1 (tail lst2) (curKey - spr1 + shift elt)
  where
    mergeTwoBoundary :: Integer -> Integer -> Integer
    mergeTwoBoundary m1 m2 = div (factorial (m1 + m2)) (factorial m1 * factorial m2)
    spr1 = mergeTwoBoundary (len lst1 - 1) (len lst2)
    spr2 = mergeTwoBoundary (len lst1) (len lst2 - 1)
    curKey = mod key (spr1 + spr2)

mergeLists :: (Shifting a) => [[a]] -> Integer -> [a]
mergeLists [] _ = []
mergeLists [l] _ = l
mergeLists [l1, l2] key = mergeTwoLists l1 l2 key
mergeLists (l : ls) key = mergeTwoLists l (mergeLists ls keyMod) nextKey
  where
    (keyDiv, keyMod) = divMod key $ mergeListsSpread $ map len ls
    nextKey = keyDiv + shift l

mergeListsSpread :: [Integer] -> Integer
mergeListsSpread amts = div (factorial $ sum amts) (product $ map factorial amts)

getChoiceAndMerge :: (Eq a, Shifting a) => [([a], Integer)] -> Integer -> [a]
getChoiceAndMerge =
  let chooseSpread' = chooseSpread . dropElementInfo
   in composeHashing' (mapHashing chooseOrdered chooseSpread') (product . map chooseSpread') mergeLists

-- Get a hash sequence from a key and a source configuration
getHash :: (Eq a, Shifting a) => [([a], Integer)] -> Integer -> Integer -> [a]
getHash = composeHashing getChoiceAndMerge shuffleList
  where
    shuffleList :: (Eq a, Shifting a) => [a] -> Integer -> [a]
    shuffleList src = chooseOrdered (src, len src)

-- ┌──────────────┐
-- │ READING KEYS │
-- └──────────────┘

getPublicKey :: String -> Integer
getPublicKey "" = 0
getPublicKey (c : cs) = toInteger (ord c) * (128 ^ length cs) + getPublicKey cs

breakAtPower :: String -> (String, String)
breakAtPower s = (s1, s2')
  where
    (s1, s2) = break (== '-') s
    s2' = if null s2 then s2 else tail s2

getPrivateKey :: String -> Integer
getPrivateKey s = base ^ pow
  where
    (baseStr, powStr) = breakAtPower s
    base :: Integer
    base = read baseStr
    pow :: Integer
    pow = if null powStr then 1 else read powStr

-- ┌──────────────────┐
-- │ COUNTING NUMBERS │
-- └──────────────────┘

numberOfHashes :: [(Integer, Integer)] -> Integer
numberOfHashes amts = product (zipWith cnk fsts snds) * factorial (sum snds)
  where
    fsts = map fst amts
    snds = map snd amts

numberOfPrivateChoiceKeys :: [(Integer, Integer)] -> Integer
numberOfPrivateChoiceKeys amts = (product . map chooseSpread) amts * (mergeListsSpread . map snd) amts

numberOfPrivateShuffleKeys :: [Integer] -> Integer
numberOfPrivateShuffleKeys = factorial . sum

numberOfRepetitions :: [Integer] -> Integer
numberOfRepetitions = numberOfPrivateShuffleKeys

numberOfPublicKeys :: [(Integer, Integer)] -> Integer
numberOfPublicKeys = numberOfPrivateChoiceKeys

maxLengthOfPublicKey :: [(Integer, Integer)] -> Integer
maxLengthOfPublicKey amts = getBiggestPower 0 $ (len . show) (numberOfPublicKeys amts)
  where
    get128PowerLength :: Integer -> Integer
    get128PowerLength p = 2 * p + 3 * pDiv + npMod
      where
        (pDiv, pMod) = divMod p 28
        npMod
          | pMod < 10 = 1
          | pMod < 19 = 2
          | otherwise = 3
    getBiggestPower :: Integer -> Integer -> Integer
    getBiggestPower guess bound
      | get128PowerLength (guess + 1) < bound = getBiggestPower (guess + 1) bound
      | otherwise = guess + 1

timeToCheck :: Double
timeToCheck = 1.0

psInYear :: Double
psInYear = 3.15576E19

ageOfUniverse :: Double
ageOfUniverse = 13.787E9

timeToCrack :: Integer -> (Integer, Integer)
timeToCrack num = (floor inYears, floor inAgesOfUniverse)
  where
    inYears = (timeToCheck / psInYear) * fromIntegral num
    inAgesOfUniverse = inYears / ageOfUniverse

formatNumber :: String -> String
formatNumber num = reverse $ formatReversed (reverse num)
  where
    formatReversed :: String -> String
    formatReversed (a : b : c : d : str) = a : b : c : ',' : formatReversed (d : str)
    formatReversed str = str

-- ┌────────────────┐
-- │ USER INTERFACE │
-- └────────────────┘

infoAction :: String -> [(Integer, Integer)] -> IO ()
infoAction "help" _ = do
      putStrLn "usage: pshash [--help | -[d|s|c|i] ARGUMENT | PUBLIC CHOICE SHUFFLE]"
      putStrLn ""
      putStrLn "options:"
      putStrLn "  --help              show this help message and exit"
      putStrLn "  -d KEYWORD          specify the default configuration. KEYWORD can be one of the following:"
      putStrLn "                          long (8 upper case, 8 lower case, 5 special characters, 4 digits) -- the default"
      putStrLn "                          medium (5 symbols of each type)"
      putStrLn "                          short (4 symbols of each type)"
      putStrLn "                          anlong (7 upper case, 7 lower case, 7 digits)"
      putStrLn "                          anshort (4 upper case, 4 lower case, 4 digits)"
      putStrLn "                          pin (4-digit pin code)"
      putStrLn "                          mediumpin (6-digit pin code)"
      putStrLn "                          longpin (8-digit pin code)"
      putStrLn "  -s \"(L, U, S, D)\"   specify how many Lower case, Upper case, Special characters, and Digits should be used"
      putStrLn "  -c CONFIGURATION    specify the configuration manually"
      putStrLn "  -i KEYWORD          show help information. KEYWORD can be one of the following:"
      putStrLn "                          numbers (show the number of hashes and keys)"
      putStrLn "                          times (show the times required to crack your passwords)"
      putStrLn "                          help (show this help message)"
      putStrLn ""
      putStrLn "main arguments:"
      putStrLn "  PUBLIC stands for public key, a memorable string indicative of the password destination"
      putStrLn "  CHOICE stands for choice private key, one of 2 private keys known only to the user"
      putStrLn "  SHUFFLE stands for shuffle private key, used to encrypt the choice key"
      putStrLn ""
      putStrLn "default configuration (in the absence of -d or -c options):"
      putStrLn $ "  " ++ show defaultConfiguration
infoAction "version" _ = putStrLn currentVersion
infoAction "numbers" amts = do
      putStrLn $
        "total theoretical number of hashes:         "
          ++ formatNumber (show $ numberOfHashes amts)
      putStrLn $
        "number of choice keys:                      "
          ++ formatNumber (show $ numberOfPrivateChoiceKeys amts)
      putStrLn $
        "number of shuffle keys:                     "
          ++ formatNumber (show $ numberOfPrivateShuffleKeys $ map snd amts)
      putStrLn $
        "number of key pairs with the same hash:     "
          ++ formatNumber (show $ numberOfRepetitions $ map snd amts)
      putStrLn $ "total hash length:                          " ++ show ((sum . map snd) amts) ++ " symbols"
      putStrLn $ "maximum relevant length of the public key:  " ++ show (maxLengthOfPublicKey amts) ++ " symbols"
infoAction "times" amts = do
      putStrLn $ "assumed time to check one private key:      " ++ "1 picosecond = 10^(-12) s"
      putStrLn $
        let (inY, inAoU) = timeToCrack $ numberOfHashes amts
         in "time to brute-force your password:          "
              ++ formatNumber (show inY)
              ++ " years\n"
              ++ "                                         or "
              ++ formatNumber (show inAoU)
              ++ " ages of the Universe"
      putStrLn $
        let (inY, inAoU) = timeToCrack $ numberOfRepetitions $ map snd amts
         in "time to retrieve the keys based on a hash:  "
              ++ formatNumber (show inY)
              ++ " years\n"
              ++ "                                         or "
              ++ formatNumber (show inAoU)
              ++ " ages of the Universe"
infoAction _ _ = putStrLn "error: info command not recognized"

hashAction :: String -> String -> String -> [([Char], Integer)] -> IO ()
hashAction publicStr pcs pss config = putStrLn $ getHash config privateChoiceKey privateShuffleKey
  where
    publicKey :: Integer
    publicKey = getPublicKey publicStr
    privateChoiceKey :: Integer
    privateChoiceKey = mod (publicKey + getPrivateKey pcs) $ (numberOfPrivateChoiceKeys . map dropElementInfo) config
    privateShuffleKey :: Integer
    privateShuffleKey = getPrivateKey pss

parseArgs :: [String] -> (Bool, Bool, Bool) -> Map String String
parseArgs [] _ = empty
parseArgs ("-d" : s : rest) trp = insert "default" s $ parseArgs rest trp
parseArgs ("-s" : s : rest) trp = insert "select" s $ parseArgs rest trp
parseArgs ("-c" : s : rest) trp = insert "config" s $ parseArgs rest trp
parseArgs ("-i" : s : rest) trp = insert "info" s $ parseArgs rest trp
parseArgs ("--help" : rest) trp = insert "info" "help" $ parseArgs rest trp
parseArgs ("--version" : rest) trp = insert "info" "version" $ parseArgs rest trp
parseArgs (s : rest) (b1, b2, b3)
  | b3 = parseArgs rest (b1, b2, b3)
  | b2 = insert "shuffle" s $ parseArgs rest (True, True, True)
  | b1 = insert "choice" s $ parseArgs rest (True, True, False)
  | otherwise = insert "public" s $ parseArgs rest (True, False, False)

main :: IO ()
main = do
  args <- getArgs
  let parsedArgs :: Map String String
      parsedArgs = parseArgs args (False, False, False)
      config :: [([Char], Integer)]
      config
        | member "default" parsedArgs = case parsedArgs ! "default" of
            "long" -> defaultConfiguration
            "medium" -> mediumConfiguration
            "short" -> shortConfiguration
            "anlong" -> anlongConfiguration
            "anshort" -> anshortConfiguration
            "pin" -> pinCodeConfiguration
            "mediumpin" -> mediumPinCodeConfiguration
            "longpin" -> longPinCodeConfiguration
            _ -> defaultConfiguration
        | member "select" parsedArgs = let (a,b,c,d) = read (parsedArgs ! "select")
                                        in [(sourceLower, a), (sourceUpper, b), (sourceSpecial, c), (sourceNumbers, d)]
        | member "config" parsedArgs = read (parsedArgs ! "config")
        | otherwise = defaultConfiguration
      amts :: [(Integer, Integer)]
      amts = map dropElementInfo config
  let getKey :: String -> IO String
      getKey str = if member str parsedArgs
                   then return $ parsedArgs ! str
                   else getLine
  if member "info" parsedArgs
  then infoAction (parsedArgs ! "info") amts
  else do
    publicKey <- getKey "public"
    choiceKey <- getKey "choice"
    shuffleKey <- getKey "shuffle"
    hashAction publicKey choiceKey shuffleKey config
