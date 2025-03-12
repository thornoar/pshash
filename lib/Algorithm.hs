module Algorithm where

import Data.Char (ord, chr)

-- ┌───────────────────────────┐
-- │ GENERAL-PURPOSE FUNCTIONS │
-- └───────────────────────────┘

factorial :: Integer -> Integer
factorial n = product [1..n]

factorial' :: Integer -> Integer -> Integer
factorial' n m = product [(n-m+1)..n]

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

break' :: (Eq a) => a -> [a] -> ([a], [a])
break' _ [] = ([],[])
break' a (a':rest)
  | a == a' = ([], rest)
  | otherwise = let (res1, res2) = break' a rest in (a' : res1, res2)

dropElementInfo :: ([a], Integer) -> (Integer, Integer)
dropElementInfo (src, m) = (length' src, m)

shiftString :: Integer -> String -> String
shiftString amt = map (chr . fromInteger . (`mod` 128) . (+ amt) . toInteger . ord)

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
