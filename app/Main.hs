{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# LANGUAGE TupleSections #-}
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Use infix" #-}
module Main where

import Data.Char (ord, chr)
import Data.Map (Map, empty, insert, member, (!))
import System.Environment (getArgs)
import Data.List (elemIndex)
import Data.Maybe (fromJust)

currentVersion :: String
currentVersion = "0.1.4.0"

-- ┌───────────────────────────┐
-- │ GENERAL-PURPOSE FUNCTIONS │
-- └───────────────────────────┘

factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)

factorial' :: Integer -> Integer -> Integer
factorial' _ 0 = 1
factorial' n m = (n - (m - 1)) * factorial' n (m - 1)

cnk :: Integer -> Integer -> Integer
cnk n k = div (factorial' n k) (factorial k)

length' :: [a] -> Integer
length' = toInteger . length

insertAtIndex :: [a] -> a -> Integer -> [a]
insertAtIndex lst a 0 = a:lst
insertAtIndex (l:lst) a n = l : insertAtIndex lst a (n-1)
insertAtIndex _ _ _ = []

dropElementInfo :: ([a], Integer) -> (Integer, Integer)
dropElementInfo (src, m) = (length' src, m)

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

combineHashing :: (a -> Integer -> b) -> (b -> Integer -> c) -> (a -> Integer -> Integer -> c)
combineHashing f g a key1 key2 = g (f a key1) key2

composeHashing :: (Shifting b) => (a -> Integer -> b) -> (a -> Integer) -> (b -> Integer -> c) -> (a -> Integer -> c)
composeHashing f spr g a key = g b nextKey
  where
    (keyDiv, keyMod) = divMod key $ spr a
    b = f a keyMod
    nextKey = keyDiv + shift b

-- ┌───────────────────────────┐
-- │ HASH GENERATING FUNCTIONS │
-- └───────────────────────────┘

chooseOrdered :: (Eq a, Shifting a) => ([a], Integer) -> Integer -> [a]
chooseOrdered (_, 0) _ = []
chooseOrdered ([], _) _ = []
chooseOrdered (src, m) key = curElt : chooseOrdered (filter (/= curElt) src, m - 1) nextKey
  where
    (keyDiv, keyMod) = divMod key $ length' src
    curElt = src !! fromIntegral keyMod
    nextKey = keyDiv + shift curElt

shuffleList :: (Eq a, Shifting a) => [a] -> Integer -> [a]
shuffleList src = chooseOrdered (src, length' src)

chooseOrderedSpread :: (Integer, Integer) -> Integer
chooseOrderedSpread (n, m) = factorial' n m

chooseOrderedSpread' :: ([a], Integer) -> Integer
chooseOrderedSpread' = chooseOrderedSpread . dropElementInfo

mergeTwoLists :: (Shifting a) => ([a], [a]) -> Integer -> [a]
mergeTwoLists ([], lst2) _ = lst2
mergeTwoLists (lst1, []) _ = lst1
mergeTwoLists (lst1, lst2) key
  | curKey < spr1 =
      let elt = head lst1
       in elt : mergeTwoLists (tail lst1, lst2) (curKey + shift elt)
  | otherwise =
      let elt = head lst2
       in elt : mergeTwoLists (lst1, tail lst2) (curKey - spr1 + shift elt)
  where
    spr1 = mergeTwoListsSpread (length' lst1 - 1, length' lst2)
    spr2 = mergeTwoListsSpread (length' lst1, length' lst2 - 1)
    curKey = mod key (spr1 + spr2)

mergeTwoListsSpread :: (Integer, Integer) -> Integer
mergeTwoListsSpread (m1, m2) = div (factorial (m1 + m2)) (factorial m1 * factorial m2)

mergeTwoListsSpread' :: ([a], [a]) -> Integer
mergeTwoListsSpread' (lst1, lst2) = mergeTwoListsSpread (length' lst1, length' lst2)

mergeLists :: (Shifting a) => [[a]] -> Integer -> [a]
mergeLists [] _ = []
mergeLists [l] _ = l
mergeLists [l1, l2] key = mergeTwoLists (l1, l2) key
mergeLists (l:ls) key = mergeTwoLists (l, mergeLists ls keyMod) nextKey
  where
    (keyDiv, keyMod) = divMod key $ mergeListsSpread $ map length' ls
    nextKey = keyDiv + shift l

mergeListsSpread :: [Integer] -> Integer
mergeListsSpread amts = div (factorial $ sum amts) (product $ map factorial amts)

mergeListsSpread' :: [[a]] -> Integer
mergeListsSpread' = mergeListsSpread . map length'

chooseAndMerge :: (Eq a, Shifting a) => [([a], Integer)] -> Integer -> [a]
chooseAndMerge = composeHashing
  (mapHashing chooseOrdered chooseOrderedSpread')
  (product . map chooseOrderedSpread')
  mergeLists

chooseAndMergeSpread :: [(Integer, Integer)] -> Integer
chooseAndMergeSpread amts = (product . map chooseOrderedSpread) amts * (mergeListsSpread . map snd) amts

chooseAndMergeSpread' :: [([a], Integer)] -> Integer
chooseAndMergeSpread' = chooseAndMergeSpread . map dropElementInfo

getHash :: (Eq a, Shifting a) => [([a], Integer)] -> Integer -> Integer -> [a]
getHash = combineHashing chooseAndMerge shuffleList

-- ┌───────────────────┐
-- │ INVERSE FUNCTIONS │
-- └───────────────────┘

mapHashingI :: (Shifting b) => (a -> b -> Integer) -> (a -> Integer) -> ([a] -> [b] -> Integer)
mapHashingI _ _ [] _ = 0
mapHashingI _ _ _ [] = 0
mapHashingI fI spr (a:as) (b:bs) =
  let curSpr = spr a
      restSpr = product $ map spr as
   in fI a b + curSpr * mod (mapHashingI fI spr as bs - shift b) restSpr

combineHashingI :: (a -> Integer -> b) -> (b -> c -> Integer) -> (a -> c -> Integer -> Integer)
combineHashingI f gI a c key1 = gI (f a key1) c

combineHashingI' :: (a -> b -> Integer) -> (c -> Integer -> b) -> (a -> c -> Integer -> Integer)
combineHashingI' fI gI' a c key2 = fI a (gI' c key2)

composeHashingI :: (Shifting b) => (a -> b -> Integer) -> (a -> Integer) -> (b -> c -> Integer) -> (b -> Integer) -> (a -> c -> b) -> (a -> c -> Integer)
composeHashingI fI sprF gI sprG getB a c =
  let b = getB a c
      keyMod = fI a b
      nextKey = gI b c
   in keyMod + sprF a * mod (nextKey - shift b) (sprG b)

chooseOrderedI :: (Shifting a, Eq a) => ([a], Integer) -> [a] -> Integer
chooseOrderedI _ [] = 0
chooseOrderedI (src, _) (a:as) =
  let srcLen = length' src
      keyMod = toInteger . fromJust $ elemIndex a src
      prevSpread = chooseOrderedSpread (srcLen-1, length' as)
      keyDiv = chooseOrderedI (filter (/= a) src, -1) as
   in keyMod + srcLen * mod (keyDiv - shift a) prevSpread

shuffleListI :: (Shifting a, Eq a) => [a] -> [a] -> Integer
shuffleListI = chooseOrderedI . (,-1)

shuffleListI' :: (Shifting a, Eq a) => [a] -> Integer -> [a]
shuffleListI' [] _ = []
shuffleListI' (r:rest) key =
  let nextLen = length' rest
      (keyDiv, keyMod) = divMod key (nextLen + 1)
      nextKey = keyDiv + shift r
   in insertAtIndex (shuffleListI' rest nextKey) r keyMod

mergeTwoListsI :: (Shifting a, Eq a) => ([a], [a]) -> [a] -> Integer
mergeTwoListsI ([], _) _ = 0
mergeTwoListsI (_, []) _ = 0
mergeTwoListsI (e1:rest1, e2:rest2) (m:ms)
  | m == e1 =
    let prevKey = mergeTwoListsI (rest1, e2:rest2) ms
     in mod (prevKey - shift m) spr1
  | m == e2 =
    let prevKey = mergeTwoListsI (e1:rest1, rest2) ms
     in spr1 + mod (prevKey - shift m) spr2
  | otherwise = -1 -- error
  where
    spr1 = mergeTwoListsSpread (length' rest1, 1 + length' rest2)
    spr2 = mergeTwoListsSpread (1 + length' rest1, length' rest2)
mergeTwoListsI (_, _) [] = -1 -- error

mergeListsI :: (Shifting a, Eq a) => [[a]] -> [a] -> Integer
mergeListsI [] _ = 0
mergeListsI [_] _ = 0
mergeListsI [l1, l2] res = mergeTwoListsI (l1,l2) res
mergeListsI (l:ls) res =
  let curSpr = mergeListsSpread' ls
      nextSpr = mergeTwoListsSpread (length' l, sum $ map length' ls)
      resWithoutL = filter (`notElem` l) res
      keyMod = mergeListsI ls resWithoutL
      nextKey = mergeTwoListsI (l, resWithoutL) res
   in keyMod + curSpr * mod (nextKey - shift l) nextSpr

distribute :: (Eq a) => [[a]] -> [[a]] -> a -> [[a]]
distribute [] _ _ = []
distribute _ [] _ = []
distribute (src:srcRest) (res:resRest) a
  | elem a src = (a:res) : resRest
  | otherwise = res : distribute srcRest resRest a

invertMergeLists :: (Eq a) => [[a]] -> [a] -> [[a]]
invertMergeLists srcs [] = [[] | _ <- srcs]
invertMergeLists srcs (a:as) = distribute srcs (invertMergeLists srcs as) a

chooseAndMergeI :: (Shifting a, Eq a) => [([a], Integer)] -> [a] -> Integer
chooseAndMergeI = composeHashingI
  (mapHashingI chooseOrderedI chooseOrderedSpread')
  (product . map chooseOrderedSpread')
  mergeListsI
  mergeListsSpread'
  (invertMergeLists . map fst)

getHashI :: (Shifting a, Eq a) => [([a], Integer)] -> [a] -> Integer -> Integer
getHashI = combineHashingI chooseAndMerge shuffleListI

getHashI' :: (Shifting a, Eq a) => [([a], Integer)] -> [a] -> Integer -> Integer
getHashI' = combineHashingI' chooseAndMergeI shuffleListI'

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

-- ┌──────────────┐
-- │ READING KEYS │
-- └──────────────┘

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

numberOfHashes' :: [([Char], Integer)] -> Integer
numberOfHashes' = numberOfHashes . map dropElementInfo

numberOfChoiceKeys :: [(Integer, Integer)] -> Integer
numberOfChoiceKeys = chooseAndMergeSpread

numberOfChoiceKeys' :: [([a], Integer)] -> Integer
numberOfChoiceKeys' = chooseAndMergeSpread'

numberOfShuffleKeys :: [Integer] -> Integer
numberOfShuffleKeys = factorial . sum

numberOfShuffleKeys' :: [([a], Integer)] -> Integer
numberOfShuffleKeys' = numberOfShuffleKeys . map snd

numberOfRepetitions :: [Integer] -> Integer
numberOfRepetitions = numberOfShuffleKeys

numberOfPublicKeys :: [(Integer, Integer)] -> Integer
numberOfPublicKeys = numberOfChoiceKeys

numberOfPublicKeys' :: [([a], Integer)] -> Integer
numberOfPublicKeys' = numberOfChoiceKeys'

maxLengthOfPublicKey :: [(Integer, Integer)] -> Integer
maxLengthOfPublicKey amts = getBiggestPower 0 $ (length' . show) (numberOfPublicKeys amts)
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

timeToChechPicos :: Double
timeToChechPicos = 100.0

psInYear :: Double
psInYear = 3.15576E19

ageOfUniverseYears :: Double
ageOfUniverseYears = 13.787E9

timeToCrack :: Integer -> (Double, Double)
timeToCrack num = (inYears, inAgesOfUniverse)
  where
    inYears = (timeToChechPicos / psInYear) * fromIntegral num
    inAgesOfUniverse = inYears / ageOfUniverseYears

formatInteger :: String -> String
formatInteger num = reverse $ formatReversed (reverse num)
  where
    formatReversed :: String -> String
    formatReversed (a : b : c : d : str) = a : b : c : ',' : formatReversed (d : str)
    formatReversed str = str

formatDouble :: String -> Int -> String
formatDouble "" _ = ""
formatDouble ('e':'-':rest) _ = " * 10^(-" ++ rest ++ ")"
formatDouble ('e':rest) _ = " * 10^" ++ rest
formatDouble (digit:rest) places
  | places == 0 = formatDouble rest places
  | otherwise = digit : formatDouble rest (places-1)

numberOfPlaces :: Int
numberOfPlaces = 4

-- ┌─────────────────────┐
-- │ FINAL HASH FUNCTION │
-- └─────────────────────┘

getFinalHash :: [([Char], Integer)] -> String -> String -> String -> [Char]
getFinalHash config publicStr choiceStr shuffleStr = getHash config choiceKey shuffleKey
  where
    publicKey = getPublicKey publicStr
    choiceKey = mod (publicKey + getPrivateKey choiceStr) $ chooseAndMergeSpread' config
    shuffleKey = getPrivateKey shuffleStr

-- ┌─────────────────┐
-- │ QUERY FUNCTIONS │
-- └─────────────────┘

retrievePublicKey :: [([Char], Integer)] -> String -> String -> [Char] -> String
retrievePublicKey config choiceStr shuffleStr hashStr =
  let shuffleKey = getPrivateKey shuffleStr
      preChoiceKey = getPrivateKey choiceStr
      choiceKey = getHashI' config hashStr shuffleKey
   in getPublicStr $ mod (choiceKey - preChoiceKey) (numberOfPublicKeys' config)

retrieveChoiceKey :: [([Char], Integer)] -> String -> String -> [Char] -> Integer
retrieveChoiceKey config publicStr shuffleStr hashStr =
  let publicKey = getPublicKey publicStr
      shuffleKey = getPrivateKey shuffleStr
      preChoiceKey = getHashI' config hashStr shuffleKey
      choiceMergeSpr = chooseAndMergeSpread' config
   in mod (preChoiceKey - publicKey) choiceMergeSpr

retrieveShuffleKey :: [([Char], Integer)] -> String -> String -> [Char] -> Integer
retrieveShuffleKey config publicStr choiceStr hashStr =
  let publicKey = getPublicKey publicStr
      choiceKey = mod (publicKey + getPrivateKey choiceStr) $ numberOfChoiceKeys' config
   in getHashI config hashStr choiceKey

-- ┌────────────────┐
-- │ USER INTERFACE │
-- └────────────────┘

-- TODO: Update
infoAction :: [([Char], Integer)] -> String -> IO ()
infoAction config "help" = do
      putStrLn "usage: pshash [--help | --version | -[d|s|c|i|q] ARGUMENT | PUBLIC CHOICE SHUFFLE]"
      putStrLn ""
      putStrLn "options:"
      putStrLn "  --help              show this help message and exit"
      putStrLn "  --version           print the current version of pshash"
      putStrLn "  -d KEYWORD          specify the source configuration. KEYWORD can be one of the following:"
      putStrLn "                          long (8 upper case, 8 lower case, 5 special characters, 4 digits) -- the default"
      putStrLn "                          medium (5 symbols of each type)"
      putStrLn "                          short (4 symbols of each type)"
      putStrLn "                          anlong (7 upper case, 7 lower case, 7 digits)"
      putStrLn "                          anshort (4 upper case, 4 lower case, 4 digits)"
      putStrLn "                          pin (4-digit pin code)"
      putStrLn "                          mediumpin (6-digit pin code)"
      putStrLn "                          longpin (8-digit pin code)"
      putStrLn "  -s \"(L, U, S, D)\"   specify how many Lower case, Upper case, Special characters, and Digits should be used"
      putStrLn "  -c CONFIGURATION    specify the source configuration manually"
      putStrLn "  -i KEYWORD          show help information. KEYWORD can be one of the following:"
      putStrLn "                          numbers (show the number of hashes and keys)"
      putStrLn "                          times (show the times required to crack your passwords)"
      putStrLn "                          help (show this help message)"
      putStrLn "  -q KEYWORD          retrieve one of the keys from a final hash and two remaining keys. KEYWORD can be:"
      putStrLn "                          public (followed by CHOICE SHUFFLE HASH)"
      putStrLn "                          choice (followed by PUBLIC SHUFFLE HASH)"
      putStrLn "                          shuffle (followed by PUBLIC CHOICE HASH)"
      putStrLn "  -l PUBLIC           print the list of (choice,shuffle) pairs that would produce the given hash."
      putStrLn ""
      putStrLn "main arguments:"
      putStrLn "  PUBLIC stands for public key, a memorable string indicative of the password destination (e.g. \"google\", \"steam\")"
      putStrLn $
        let numChoiceDouble = fromIntegral (numberOfChoiceKeys' config) :: Double
         in "  CHOICE stands for choice private key, a large number between 0 and " ++ formatDouble (show numChoiceDouble) numberOfPlaces
      putStrLn $
        let numShuffleDouble = fromIntegral (numberOfShuffleKeys $ map snd config) :: Double
         in "  SHUFFLE stands for shuffle private key, a number between 0 and " ++ formatDouble (show numShuffleDouble) numberOfPlaces
      putStrLn ""
      putStrLn "using source configuration:"
      putStrLn $ "  " ++ show config
infoAction _ "version" = putStrLn currentVersion
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
      ++ formatInteger (show numHashes) ++ " = "
      ++ formatDouble (show numHashesDouble) numberOfPlaces
  putStrLn $
    "number of choice keys:                      "
      ++ formatInteger (show numChoice) ++ " = "
      ++ formatDouble (show numChoiceDouble) numberOfPlaces
  putStrLn $
    "number of shuffle keys:                     "
      ++ formatInteger (show numShuffle) ++ " = "
      ++ formatDouble (show numShuffleDouble) numberOfPlaces
  putStrLn $
    "number of key pairs with the same hash:     "
      ++ formatInteger (show numRepetitions) ++ " = "
      ++ formatDouble (show numRepetitionsDouble) numberOfPlaces
  putStrLn $ "total hash length:                          " ++ show ((sum . map snd) amts) ++ " symbols"
  putStrLn $ "maximum relevant length of the public key:  " ++ show (maxLengthOfPublicKey amts) ++ " symbols"
infoAction config "times" = let amts = map dropElementInfo config in do
      putStrLn $ "using the following distribution:               " ++ show amts
      putStrLn $ "assumed number of password checks per second:   " ++ "10 billion = 10^10"
      putStrLn $ "time to check one password:                     " ++ "10^(-10) s = 0.1 nanosecond"
      putStrLn ""
      putStrLn $
        let (inY, inAoU) = timeToCrack $ numberOfHashes amts
            inAoUInteger = floor inAoU :: Integer
            inYInteger = floor inY :: Integer
         in "time to brute-force your password:              "
              ++ formatInteger (show inYInteger) ++ " = "
              ++ formatDouble (show inY) numberOfPlaces ++ " years\n"
              ++ "                                             or "
              ++ formatInteger (show inAoUInteger) ++ " = "
              ++ formatDouble (show inAoU) numberOfPlaces
              ++ " ages of the Universe"
      putStrLn $
        let (inY, inAoU) = timeToCrack $ numberOfRepetitions $ map snd amts
            inAoUInteger = floor inAoU :: Integer
            inYInteger = floor inY :: Integer
         in "time to retrieve the keys based on a hash:      "
              ++ formatInteger (show inYInteger) ++ " = "
              ++ formatDouble (show inY) numberOfPlaces ++ " years\n"
              ++ "                                             or "
              ++ formatInteger (show inAoUInteger) ++ " = "
              ++ formatDouble (show inAoU) numberOfPlaces
              ++ " ages of the Universe"
infoAction _ cmd = putStrLn $ "error: info command not recognized: " ++ cmd

queryAction :: [([Char], Integer)] -> String -> [Char] -> String -> String -> IO ()
queryAction config "public" choiceStr shuffleStr hashStr = putStrLn $ retrievePublicKey config choiceStr shuffleStr hashStr
queryAction config "choice" publicStr shuffleStr hashStr = print $ retrieveChoiceKey config publicStr shuffleStr hashStr
queryAction config "shuffle" publicStr choiceStr hashStr = print $ retrieveShuffleKey config publicStr choiceStr hashStr
queryAction config "hash" choiceStr shuffleStr publicStr = putStrLn $ getFinalHash config publicStr choiceStr shuffleStr
queryAction _ kw _ _ _ = putStrLn $ "error: query keyword not recognized: " ++ kw

listPairsAction :: [([Char], Integer)] -> String -> [Char] -> IO ()
listPairsAction config publicStr hashStr =
  let publicKey = getPublicKey publicStr
      g :: Integer -> IO ()
      g key = putStrLn $ show (mod (getHashI' config hashStr key - publicKey) (numberOfChoiceKeys' config)) ++ " " ++ show key
   in mapM_ g [0 .. numberOfShuffleKeys' config - 1]

hashAction :: [([Char], Integer)] -> String -> String -> String -> IO ()
hashAction config publicStr choiceStr shuffleStr = putStrLn $ getFinalHash config publicStr choiceStr shuffleStr

parseArgs :: (Bool, Bool, Bool) -> [String] -> Map String String
parseArgs _ [] = empty
parseArgs trp ("-d" : s : rest) = insert "keyword" s $ parseArgs trp rest
parseArgs trp ("-s" : s : rest) = insert "select" s $ parseArgs trp rest
parseArgs trp ("-c" : s : rest) = insert "config" s $ parseArgs trp rest
parseArgs trp ("-i" : s : rest) = insert "info" s $ parseArgs trp rest
parseArgs trp ("-q" : s : rest) = insert "query" s $ parseArgs trp rest
parseArgs trp ("-l" : s : rest) = insert "list" s $ parseArgs trp rest
parseArgs trp ("--help" : rest) = insert "info" "help" $ parseArgs trp rest
parseArgs trp ("--version" : rest) = insert "info" "version" $ parseArgs trp rest
parseArgs (b1, b2, b3) (s : rest)
  | b3 = parseArgs (b1, b2, b3) rest
  | b2 = insert "third" s $ parseArgs (True, True, True) rest
  | b1 = insert "second" s $ parseArgs (True, True, False) rest
  | otherwise = insert "first" s $ parseArgs (True, False, False) rest

getKeyStr :: Map String String -> String -> IO String
getKeyStr args str = if member str args
                then return $ args ! str
                else getLine

passKeysToAction :: Map String String -> (String -> String -> String -> IO ()) -> IO ()
passKeysToAction args act = do
  first <- getKeyStr args "first"
  second <- getKeyStr args "second"
  third <- getKeyStr args "third"
  act first second third

getConfig :: Map String String -> [([Char], Integer)]
getConfig args
  | member "keyword" args = case args ! "keyword" of
      "long" -> defaultConfiguration
      "medium" -> mediumConfiguration
      "short" -> shortConfiguration
      "anlong" -> anlongConfiguration
      "anshort" -> anshortConfiguration
      "pin" -> pinCodeConfiguration
      "mediumpin" -> mediumPinCodeConfiguration
      "longpin" -> longPinCodeConfiguration
      _ -> defaultConfiguration
  | member "select" args = let (a,b,c,d) = read (args ! "select")
                                  in [(sourceLower, a), (sourceUpper, b), (sourceSpecial, c), (sourceNumbers, d)]
  | member "config" args = read (args ! "config")
  | otherwise = defaultConfiguration

getAction :: Map String String -> [([Char], Integer)] -> IO ()
getAction args config
  | member "info" args = infoAction config (args ! "info")
  | member "query" args = passKeysToAction args (queryAction config (args ! "query"))
  | member "list" args = getKeyStr args "first" >>= listPairsAction config (args ! "list")
  | otherwise = passKeysToAction args (hashAction config)

bindFunctions :: (a -> b) -> (b -> c) -> (b -> c -> d) -> (a -> d)
bindFunctions f g h a = let b = f a in h b (g b)

main :: IO ()
main = getArgs >>= bindFunctions (parseArgs (False, False, False)) getConfig getAction

-- ┌────────────────────┐
-- │ INVERSE VALIDATION │
-- └────────────────────┘

isInverseOnRange :: (a -> Integer -> b) -> (a -> b -> Integer) -> a -> [Integer] -> Integer -> IO ()
isInverseOnRange _ _ _ [] _ = putStrLn "| All tests passed. |"
isInverseOnRange f fI a (key:rest) lim =
  if key == fI a (f a key)
  then putStrLn (show key ++ ": Passed. " ++ show (lim - 1) ++ " left.") >> isInverseOnRange f fI a rest (lim - 1)
  else do
    putStrLn ("| Failed on number " ++ show key ++ " with:")
    putStrLn ("|   key = " ++ show key)
    putStrLn ("|   output = " ++ show (fI a (f a key)))

isInverseOnRange2 :: (a -> Integer -> Integer -> b) -> (a -> b -> Integer -> Integer) -> a -> Integer -> [Integer] -> Integer -> IO ()
isInverseOnRange2 f2 f2I a key1 key2s lim =
  let f a' = f2 a' key1
      fI a' b = f2I a' b key1
   in isInverseOnRange f fI a key2s lim

isInverseOnRange2' :: (a -> Integer -> Integer -> b) -> (a -> b -> Integer -> Integer) -> a -> Integer -> [Integer] -> Integer -> IO ()
isInverseOnRange2' f2 f2I a key2 key1s lim =
  let f a' key1 = f2 a' key1 key2
      fI a' b = f2I a' b key2
   in isInverseOnRange f fI a key1s lim

isInverse :: (a -> Integer -> b) -> (a -> Integer) -> (a -> b -> Integer) -> a -> IO ()
isInverse f sprF fI a = isInverseOnRange f fI a [0 .. sprF a - 1] (sprF a)

isInverse2 :: (a -> Integer -> Integer -> b) -> (a -> Integer) -> (a -> b -> Integer -> Integer) -> a -> Integer -> IO ()
isInverse2 f2 sprF f2I a key1 = isInverseOnRange2 f2 f2I a key1 [0 .. sprF a - 1] (sprF a)

isInverse2' :: (a -> Integer -> Integer -> b) -> (a -> Integer) -> (a -> b -> Integer -> Integer) -> a -> Integer -> IO ()
isInverse2' f2 sprF f2I a key2 = isInverseOnRange2' f2 f2I a key2 [0 .. sprF a - 1] (sprF a)
