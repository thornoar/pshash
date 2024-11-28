{-# OPTIONS_GHC -Wno-unused-imports #-}
module Main where

import Data.Char (ord, chr)
import Data.Map (Map, empty, insert, member, (!))
import System.Environment (getArgs)
import Data.List (elemIndex)
import Text.Read (readMaybe)
import Control.Applicative (liftA2)

currentVersion :: String
currentVersion = "0.1.8.1"

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

take' :: Integer -> [a] -> [a]
take' 0 _ = []
take' _ [] = []
take' n (a:as) = a : take' (n-1) as

insertAt :: a -> Integer -> [a] -> [a]
insertAt a 0 lst = a:lst
insertAt a n (l:lst) = l : insertAt a (n-1) lst
insertAt _ _ _ = []

dropElementInfo :: ([a], Integer) -> (Integer, Integer)
dropElementInfo (src, m) = (length' src, m)

class Shifting a where
  shift :: a -> Integer

instance (Shifting a) => Shifting [a] where
  shift = sum . map shift

instance Shifting Char where
  shift = toInteger . ord

combineHashing :: (a -> Integer -> b) -> (b -> Integer -> c) -> (a -> Integer -> Integer -> c)
combineHashing f g a = g . f a

mapHashing :: (Shifting b) => (a -> Integer -> b) -> (a -> Integer) -> ([a] -> Integer -> [b])
mapHashing _ _ [] _ = []
mapHashing f spr (a : as) key = b : mapHashing f spr as nextKey
  where
    (keyDiv, keyMod) = divMod key $ spr a
    b = f a keyMod
    nextKey = keyDiv + shift b

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
mergeTwoLists (elt1:rest1, elt2:rest2) key
  | curKey < spr1 = elt1 : mergeTwoLists (rest1, elt2:rest2) (curKey + shift elt1)
  | otherwise = elt2 : mergeTwoLists (elt1:rest1, rest2) (curKey - spr1 + shift elt2)
  where
    spr1 = mergeTwoListsSpread (length' rest1, length' rest2 + 1)
    spr2 = mergeTwoListsSpread (length' rest1 + 1, length' rest2)
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

-- ┌────────────────┐
-- │ ERROR HANDLING │
-- └────────────────┘

data Trace = String :=> [Trace] deriving (Read, Show)

data Handle a = Content a | Error Trace deriving (Read, Show)

red :: String -> String
red = (++ "\ESC[0m") . ("\ESC[31m" ++) -- ]]

newError :: String -> Handle a
newError = Error . (:=> []) . red

instance Functor Handle where
  fmap f ma = case ma of
    Error tr -> Error ("Trace in application of `fmap`:" :=> [tr])
    Content a -> Content (f a)
instance Applicative Handle where
  pure = Content
  mf <*> ma = case mf of
    Error tr -> case ma of
      Error tr' -> Error ("Double trace in application of `<*>`:" :=> [tr, tr'])
      _ -> Error ("Trace in application of `<*>`:" :=> [tr])
    Content f -> fmap f ma
instance Monad Handle where
  return = pure
  mval >>= f = case mval of
    Error tr -> Error ("Trace in application of `>>=`:" :=> [tr])
    Content a -> f a

readHandle :: (Read a) => String -> String -> Handle a
readHandle msg str = case readMaybe str of
  Nothing -> newError ("Failed to read \"" ++ str ++ "\" as " ++ msg ++ ".")
  Just a -> Content a

fmap2 :: (Monad m) => (a -> b -> c) -> (m a -> b -> m c)
fmap2 f ma b = fmap (`f` b) ma

fmap2' :: (Monad m) => (a -> b -> m c) -> (m a -> b -> m c)
fmap2' f ma b = ma >>= (`f` b)

-- ┌───────────────────┐
-- │ INVERSE FUNCTIONS │
-- └───────────────────┘

mapHashingI :: (Shifting b) => (a -> b -> Handle Integer) -> (a -> Integer) -> ([a] -> [b] -> Handle Integer)
mapHashingI _ _ [] [] = Content 0
mapHashingI _ _ _ [] = newError "Failed to map un-hashing: list of sources too long."
mapHashingI _ _ [] _ = newError "Failed to map un-hashing: list of hashes too long."
mapHashingI fI spr (a:as) (b:bs) =
  let curSpr = spr a
      restSpr = product $ map spr as
      curKeyH = fI a b
      restPreKeyH = mapHashingI fI spr as bs -- - shift b
   in case (curKeyH, restPreKeyH) of
        (Error tr1, Error tr2) -> Error ("Double trace in call of `mapHashing`:" :=> [tr1, tr2])
        (Error tr, _) -> Error ("Trace in call of `mapHashingI`:" :=> [tr])
        (_, Error tr) -> Error tr
        (Content curKey, Content nextPreKey) -> Content $ curKey + curSpr * mod (nextPreKey - shift b) restSpr

combineHashingI :: (a -> Integer -> b) -> (b -> c -> Handle Integer) -> (a -> c -> Integer -> Handle Integer)
combineHashingI f gI a c key1 = gI (f a key1) c

combineHashingI' :: (a -> b -> Handle Integer) -> (c -> Integer -> b) -> (a -> c -> Integer -> Handle Integer)
combineHashingI' fI gI' a c key2 = fI a (gI' c key2)

composeHashingI :: (Shifting b) => (a -> b -> Handle Integer) -> (a -> Integer) -> (b -> c -> Handle Integer) -> (b -> Integer) -> (a -> c -> b) -> (a -> c -> Handle Integer)
composeHashingI fI sprF gI sprG getB a c =
  let b = getB a c
      keyModH = fI a b
      nextKeyH = gI b c
   in case (keyModH, nextKeyH) of
        (Error tr1, Error tr2) -> Error ("Double trace in call of `composeHashingI`:" :=> [tr1, tr2])
        (Error tr, _) -> Error ("Trace in call of `composeHashingI`, in first function:" :=> [tr])
        (_, Error tr) -> Error ("Trace in call of `composeHashingI`, in second function:" :=> [tr])
        (Content keyMod, Content nextKey) -> Content $ keyMod + sprF a * mod (nextKey - shift b) (sprG b)

chooseOrderedI :: (Shifting a, Eq a, Show a) => ([a], Integer) -> [a] -> Handle Integer
chooseOrderedI (_,0) [] = Content 0
chooseOrderedI (src,num) hash
  | num /= length' hash = Error $
      red "Invalid hash: length should match source configuration." :=>
      [
        ("Reversing hash: " ++ show hash ++ " with length " ++ show (length hash)) :=> [],
        ("With source: " ++ show (src,num)) :=> []
      ]  
chooseOrderedI (src, num) (a:as) =
  let srcLen = length' src
      keyModM = toInteger <$> elemIndex a src
      prevSpread = chooseOrderedSpread (srcLen-1, length' as)
      keyDivH = chooseOrderedI (filter (/= a) src, num-1) as
   in case keyModM of
        Nothing -> Error $
          red ("Invalid hash: element " ++ show a ++ " could not be found in source.") :=>
          [
            ("Perhaps element " ++ show a ++ " is repeated in the hash?") :=> [],
            "Perhaps the hash is incompatible with choice key?" :=> []
          ]
        Just keyMod -> case keyDivH of
          Error tr -> Error tr
          Content keyDiv -> Content $ keyMod + srcLen * mod (keyDiv - shift a) prevSpread
chooseOrderedI (_,_) _ = newError "Invalid hash. Why? IDFK."

shuffleListI :: (Shifting a, Eq a, Show a) => [a] -> [a] -> Handle Integer
shuffleListI lst = chooseOrderedI (lst, length' lst)

shuffleListI' :: (Shifting a, Eq a) => [a] -> Integer -> [a]
shuffleListI' [] _ = []
shuffleListI' (r:rest) key =
  let nextLen = length' rest
      (keyDiv, keyMod) = divMod key (nextLen + 1)
      nextKey = keyDiv + shift r
   in insertAt r keyMod (shuffleListI' rest nextKey)

mergeTwoListsI :: (Shifting a, Eq a, Show a) => ([a], [a]) -> [a] -> Handle Integer
mergeTwoListsI ([], src) hash
  | src == hash = Content 0
  | otherwise = Error $
      red "Invald hash: consists of different elements than source." :=>
      [
        ("Reversing hash: " ++ show hash) :=> [],
        ("Using source: " ++ show src) :=> []
      ]  
mergeTwoListsI (src, []) hash
  | src == hash = Content 0
  | otherwise = Error $
      red "Invald hash: consists of different elements than source." :=>
      [
        ("Reversing hash: " ++ show hash) :=> [],
        ("Using source: " ++ show src) :=> []
      ]  
mergeTwoListsI (e1:rest1, e2:rest2) (m:ms)
  | m == e1 = case mergeTwoListsI (rest1, e2:rest2) ms of
      Error tr -> Error tr
      Content prevKey -> Content $ mod (prevKey - shift m) spr1
  | m == e2 = case mergeTwoListsI (e1:rest1, rest2) ms of
      Error tr -> Error tr
      Content prevKey -> Content $ spr1 + mod (prevKey - shift m) spr2
  | otherwise = Error $
      red ("Invalid hash: element " ++ show m ++ " does not match either source.") :=>
      [
        ("Reversing hash: " ++ show (m:ms)) :=> [],
        ("First source list: " ++ show (e1:rest1)) :=> [],
        ("Second source list: " ++ show (e2:rest2)) :=> [],
        ("The head of the hash should be either " ++ show e1 ++ " or " ++ show e2) :=> []
      ]  
  where
    spr1 = mergeTwoListsSpread (length' rest1, 1 + length' rest2)
    spr2 = mergeTwoListsSpread (1 + length' rest1, length' rest2)
mergeTwoListsI (_, _) [] = newError "Invalid hash: too few elements."

mergeListsI :: (Shifting a, Eq a, Show a) => [[a]] -> [a] -> Handle Integer
mergeListsI [] [] = Content 0
mergeListsI [] _  = newError "Invalid hash: too many characters."
mergeListsI [src] lst
  | lst == src = Content 0
  | otherwise = Error $
      red "Invalid hash: element mismatch." :=>
      [
        ("Reversing hash: " ++ show lst) :=> [],
        ("Current source: " ++ show src) :=> []
      ]
mergeListsI [l1, l2] res = mergeTwoListsI (l1,l2) res
mergeListsI (l:ls) res =
  let curSpr = mergeListsSpread' ls
      nextSpr = mergeTwoListsSpread (length' l, sum $ map length' ls)
      resWithoutL = filter (`notElem` l) res
      keyModH = mergeListsI ls resWithoutL
      nextKeyH = mergeTwoListsI (l, resWithoutL) res
   in case (keyModH, nextKeyH) of
        (Error tr1, Error tr2) -> Error ("Double trace in call of `mergeListsI`:" :=> [tr1, tr2])
        (_, Error tr) -> Error ("Trace in call of `mergeListsI`:" :=> [tr])
        (Error tr, _) -> Error ("Trace in call of `mergeListsI`:" :=> [tr])
        (Content keyMod, Content nextKey) -> Content $ keyMod + curSpr * mod (nextKey - shift l) nextSpr

distribute :: (Eq a) => [[a]] -> [[a]] -> a -> [[a]]
distribute [] _ _ = []
distribute _ [] _ = []
distribute (src:srcRest) (res:resRest) a
  | a `elem` src = (a:res) : resRest
  | otherwise = res : distribute srcRest resRest a

invertMergeLists :: (Eq a) => [[a]] -> [a] -> [[a]]
invertMergeLists srcs [] = [[] | _ <- srcs]
invertMergeLists srcs (a:as) = distribute srcs (invertMergeLists srcs as) a

chooseAndMergeI :: (Shifting a, Eq a, Show a) => [([a], Integer)] -> [a] -> Handle Integer
chooseAndMergeI = composeHashingI
  (mapHashingI chooseOrderedI chooseOrderedSpread')
  (product . map chooseOrderedSpread')
  mergeListsI
  mergeListsSpread'
  (invertMergeLists . map fst)

getHashI :: (Shifting a, Eq a, Show a) => [([a], Integer)] -> [a] -> Integer -> Handle Integer
getHashI = combineHashingI chooseAndMerge shuffleListI

getHashI' :: (Shifting a, Eq a, Show a) => [([a], Integer)] -> [a] -> Integer -> Handle Integer
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
    s2' = if null s2 then s2 else drop 1 s2

getPrivateKey :: String -> Handle Integer
getPrivateKey s = liftA2 (^) base pow
  where
    (baseStr, powStr) = breakAtPower s
    base :: Handle Integer
    base = readHandle "base in private key" baseStr
    pow :: Handle Integer
    pow = if null powStr then Content 1 else case readHandle "exponent in private key" powStr of
      Error tr -> Error tr
      Content n ->
        if n < 0
        then newError ("Cannot have negative exponent in private key: " ++ powStr ++ ".")
        else Content n

-- ┌─────────────────────┐
-- │ FINAL HASH FUNCTION │
-- └─────────────────────┘

getFinalHash :: [([Char], Integer)] -> String -> String -> String -> Handle [Char]
getFinalHash config publicStr choiceStr shuffleStr = liftA2 (getHash config) choiceKey shuffleKey
  where
    publicKey = getPublicKey publicStr
    choiceKey = liftA2 mod ((+publicKey) <$> getPrivateKey choiceStr) $ return (chooseAndMergeSpread' config)
    shuffleKey = getPrivateKey shuffleStr

-- ┌─────────────────┐
-- │ QUERY FUNCTIONS │
-- └─────────────────┘

checkConfigValidity :: [([Char], Integer)] -> Handle [([Char], Integer)]
checkConfigValidity [] = newError "Cannot have empty configuration."
checkConfigValidity [(lst, num)]
  | num < 0 = newError "Invalid configuration: numbers must be non-negative."
  | num > length' lst = newError ("Invalid configuration: too many elements drawn from \"" ++ lst ++ "\".")
  | otherwise = Content [(lst, num)]
checkConfigValidity (src : rest) = case checkConfigValidity [src] of
  Error tr -> Error tr
  Content src' -> (src' ++) <$> checkConfigValidity rest

getConfigFromSpec :: (Integer,Integer,Integer,Integer) -> [([Char],Integer)]
getConfigFromSpec (a,b,c,d) = [(sourceLower, a), (sourceUpper, b), (sourceSpecial, c), (sourceNumbers, d)]

retrievePublicKey :: [([Char], Integer)] -> String -> String -> [Char] -> Handle String
retrievePublicKey config choiceStr shuffleStr hashStr =
  let shuffleKey = getPrivateKey shuffleStr
      preChoiceKey = getPrivateKey choiceStr
      choiceKey = shuffleKey >>= getHashI' config hashStr
   in getPublicStr <$> fmap2 mod (liftA2 (-) choiceKey preChoiceKey) (numberOfPublicKeys' config)

retrieveChoiceKey :: [([Char], Integer)] -> String -> String -> [Char] -> Handle Integer
retrieveChoiceKey config publicStr shuffleStr hashStr =
  let publicKey = getPublicKey publicStr
      shuffleKey = getPrivateKey shuffleStr
      preChoiceKey = shuffleKey >>= getHashI' config hashStr
      choiceMergeSpr = chooseAndMergeSpread' config
   in fmap2 mod (fmap2 (-) preChoiceKey publicKey) choiceMergeSpr

retrieveShuffleKey :: [([Char], Integer)] -> String -> String -> [Char] -> Handle Integer
retrieveShuffleKey config publicStr choiceStr hashStr =
  let publicKey = getPublicKey publicStr
      preChoiceKey = getPrivateKey choiceStr
      choiceKey = fmap2 mod (fmap (+ publicKey) preChoiceKey) (numberOfChoiceKeys' config)
      preHash = fmap (chooseAndMerge config) choiceKey
   in fmap2' shuffleListI preHash hashStr

-- ┌────────────────┐
-- │ USER INTERFACE │
-- └────────────────┘

infoAction :: [([Char], Integer)] -> String -> Handle (IO ())
infoAction config "help" = Content $ do
      putStrLn "usage: pshash [--help | --version | -[d|s|c|i|q|l] ARGUMENT | PUBLIC CHOICE SHUFFLE]"
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
      putStrLn "  -l                  print the list of (choice,shuffle) pairs that would produce the given hash."
      putStrLn "                      Accepts three arguments: the PUBLIC key, the NUMBER of pairs to compute, and the final HASH."
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
infoAction _ "version" = Content $ putStrLn currentVersion
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
   in Content $ do
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
infoAction config "times" = let amts = map dropElementInfo config in Content $ do
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
infoAction _ cmd = newError ("Info command not recognized: " ++ cmd ++ ".")

queryAction :: [([Char], Integer)] -> String -> [Char] -> String -> String -> Handle (IO ())
queryAction config "public" choiceStr shuffleStr hashStr = putStrLn <$> retrievePublicKey config choiceStr shuffleStr hashStr
queryAction config "choice" publicStr shuffleStr hashStr = print <$> retrieveChoiceKey config publicStr shuffleStr hashStr
queryAction config "shuffle" publicStr choiceStr hashStr = print <$> retrieveShuffleKey config publicStr choiceStr hashStr
queryAction _ kw _ _ _ = newError ("Query keyword not recognized: " ++ kw ++ ".")

listPairsAction :: [([Char], Integer)] -> String -> String -> [Char] -> Handle (IO ())
listPairsAction config publicStr limitStr hashStr =
  let publicKey = getPublicKey publicStr
      limit = readHandle "positive integer" limitStr :: Handle Integer
      sequence' :: [Handle (IO ())] -> IO ()
      sequence' [] = return ()
      sequence' (Error tr : _) = toIO $ Error ("Stopped pair listing due to error:" :=> [tr])
      sequence' (Content io : xs) = io >> sequence' xs
      g :: Integer -> Handle (IO ())
      g shuffleKey = case getHashI' config hashStr shuffleKey of
        Error tr -> Error tr
        Content preChoiceKey -> Content . putStrLn $ show (mod (preChoiceKey - publicKey) (numberOfChoiceKeys' config)) ++ " " ++ show shuffleKey
   in fmap sequence' $ fmap2 take' limit $ map g [0 .. numberOfShuffleKeys' config - 1]

hashAction :: [([Char], Integer)] -> String -> String -> String -> Handle (IO ())
hashAction config publicStr choiceStr shuffleStr = putStrLn <$> getFinalHash config publicStr choiceStr shuffleStr

getConfig :: Map String String -> Handle [([Char], Integer)]
getConfig args
  | member "keyword" args = case args ! "keyword" of
      "long" -> Content defaultConfiguration
      "medium" -> Content mediumConfiguration
      "short" -> Content shortConfiguration
      "anlong" -> Content anlongConfiguration
      "anshort" -> Content anshortConfiguration
      "pin" -> Content pinCodeConfiguration
      "mediumpin" -> Content mediumPinCodeConfiguration
      "longpin" -> Content longPinCodeConfiguration
      str -> newError ("Unrecognized configuration keyword: " ++ str ++ ".")
  | member "select" args =
      readHandle "\"(Int,Int,Int,Int)\"" (args ! "select")
      >>= (checkConfigValidity . getConfigFromSpec)
  | member "config" args =
      readHandle "a source configuration" (args ! "config") >>= checkConfigValidity
  | otherwise = Content defaultConfiguration

parseArgs :: (Bool, Bool, Bool) -> [String] -> Map String String
parseArgs _ [] = empty
parseArgs trp ("-d" : s : rest) = insert "keyword" s $ parseArgs trp rest
parseArgs trp ("-s" : s : rest) = insert "select" s $ parseArgs trp rest
parseArgs trp ("-c" : s : rest) = insert "config" s $ parseArgs trp rest
parseArgs trp ("-i" : s : rest) = insert "info" s $ parseArgs trp rest
parseArgs trp ("-q" : s : rest) = insert "query" s $ parseArgs trp rest
parseArgs trp ("-l" : rest) = insert "list" "" $ parseArgs trp rest
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

passKeysToAction :: Map String String -> (String -> String -> String -> Handle (IO ())) -> IO ()
passKeysToAction args act = do
  first <- getKeyStr args "first"
  second <- getKeyStr args "second"
  third <- getKeyStr args "third"
  toIO $ act first second third

printTrace :: Int -> Trace -> IO ()
printTrace lvl (msg :=> lst) = do
  let prefix = if lvl == 0 then "" else replicate (2*(lvl-1)) ' ' ++ "|_"
  putStrLn $ prefix ++ msg
  mapM_ (printTrace (lvl+1)) lst

toIO :: Handle (IO ()) -> IO ()
toIO (Error tr) = do
  putStrLn "\ESC[1;31mError:\ESC[0m" -- ]]
  printTrace 0 tr
toIO (Content action) = action

performAction :: Map String String -> Handle [([Char], Integer)] -> IO ()
performAction _ (Error tr) = toIO (Error tr)
performAction args (Content config)
  | member "info" args = toIO $ infoAction config (args ! "info")
  | member "query" args = passKeysToAction args (queryAction config (args ! "query"))
  | member "list" args = passKeysToAction args (listPairsAction config)
  | otherwise = passKeysToAction args (hashAction config)

main :: IO ()
main = getArgs >>= (performAction <*> getConfig) . parseArgs (False, False, False) 
