module Inverse where

import Data.List (elemIndex)
import Algorithm
import Error

-- ┌───────────────────┐
-- │ INVERSE FUNCTIONS │
-- └───────────────────┘

mapHashingI :: (Shifting b) => (a -> b -> Result Integer) -> (a -> Integer) -> ([a] -> [b] -> Result Integer)
mapHashingI _ _ [] [] = Content 0
mapHashingI _ _ _ [] = Error $ ["<A bug in the Matrix.>" :=> []]
mapHashingI _ _ [] _ = Error $ ["<A bug in the Matrix.>" :=> []]
mapHashingI fI spr (a:as) (b:bs) = liftA2
  (\ x y -> x + spr a * mod (y - shift b) (product $ map spr as))
  (fI a b) (mapHashingI fI spr as bs)

combineHashingI :: (a -> Integer -> b) -> (b -> c -> Result Integer) -> (a -> c -> Integer -> Result Integer)
combineHashingI f gI a c key1 = gI (f a key1) c

combineHashingI' :: (a -> b -> Result Integer) -> (c -> Integer -> b) -> (a -> c -> Integer -> Result Integer)
combineHashingI' fI gI' a c key2 = fI a (gI' c key2)

composeHashingI :: (Shifting b) => (a -> b -> Result Integer) -> (a -> Integer) -> (b -> c -> Result Integer) -> (b -> Integer) -> (a -> c -> b) -> (a -> c -> Result Integer)
composeHashingI fI sprF gI sprG getB a c = let b = getB a c in liftA2
  (\ x y -> x + sprF a * mod (y - shift b) (sprG b))
  (fI a b) (gI b c)

chooseOrderedI :: (Shifting a, Eq a, Show a) => ([a], Integer) -> [a] -> Result Integer
chooseOrderedI (_,0) [] = Content 0
chooseOrderedI (src, num) hash
  | num /= length' hash = Error $ ["<Invalid pseudo-hash: length should match source configuration.>" :=> [
      ("Reversing pseudo-hash: {" ++ show hash ++ "} with length {" ++ show (length hash) ++ "}") :=> [],
      ("With source: {" ++ show (src, num) ++ "} which assumes length {" ++ show num ++ "}") :=> []
    ]]
chooseOrderedI (src, num) (a:as) = let srcLen = length' src in case toInteger <$> elemIndex a src of
  Nothing -> Error $ [("<Invalid pseudo-hash: element {{" ++ show a ++ "}} could not be found in source.>") :=> [
      ("Maybe the element {" ++ show a ++ "} is " ++ "{repeated}" ++ " in the pseudo-hash,") :=> [],
      ("Or the pseudo-hash " ++ "{is incompatible}" ++ " with the choice key?") :=> []
    ]]
  Just keyMod -> fmap
    (\x -> keyMod + srcLen * mod (x - shift a) (chooseOrderedSpread (srcLen-1, length' as)))
    (chooseOrderedI (filter (/= a) src, num-1) as)
chooseOrderedI (_, _) _ = Error $ ["<A bug in the Matrix.>" :=> []]

shuffleListI :: (Shifting a, Eq a, Show a) => [a] -> [a] -> Result Integer
shuffleListI lst = chooseOrderedI (lst, length' lst)

shuffleListI' :: (Shifting a, Eq a) => [a] -> Integer -> [a]
shuffleListI' [] _ = []
shuffleListI' (r:rest) key =
  let nextLen = length' rest
      (keyDiv, keyMod) = divMod key (nextLen + 1)
      nextKey = keyDiv + shift r
   in insertAt r keyMod (shuffleListI' rest nextKey)

mergeTwoListsI :: (Shifting a, Eq a, Show a) => ([a], [a]) -> [a] -> Result Integer
mergeTwoListsI ([], src) hash
  | src == hash = Content 0
  | otherwise = Error $ ["<Invalid pseudo-hash: element mismatch.>" :=> [
      ("Reversing pseudo-hash: {" ++ show hash ++ "}") :=> [],
      ("Using source: {" ++ show src ++ "}") :=> []
    ]]
mergeTwoListsI (src, []) hash
  | src == hash = Content 0
  | otherwise = Error $ ["<Invalid pseudo-hash: element mismatch.>" :=> [
      ("Reversing pseudo-hash: {" ++ show hash ++ "}") :=> [],
      ("Using source: {" ++ show src ++ "}") :=> []
    ]]
mergeTwoListsI (e1:rest1, e2:rest2) (m:ms)
  | m == e1 = fmap (\x -> mod (x - shift m) spr1) (mergeTwoListsI (rest1, e2:rest2) ms)
  | m == e2 = fmap (\x -> spr1 + mod (x - shift m) spr2) (mergeTwoListsI (e1:rest1, rest2) ms)
  | otherwise = Error $ [("<Invalid pseudo-hash: element {{" ++ show m ++ "}} does not match either source.>") :=> [
      ("Reversing pseudo-hash: {" ++ show (m:ms) ++ "}") :=> [],
      ("First source list: {" ++ show (e1:rest1) ++ "}") :=> [],
      ("Second source list: {" ++ show (e2:rest2) ++ "}") :=> [],
      ("The head of the pseudo-hash should be either {" ++ show e1 ++ "} or {" ++ show e2 ++ "}") :=> []
    ]]
  where
    spr1 = mergeTwoListsSpread (length' rest1, 1 + length' rest2)
    spr2 = mergeTwoListsSpread (1 + length' rest1, length' rest2)
mergeTwoListsI (_, _) [] = Error $ ["<Invalid pseudo-hash: too few elements.>" :=> []]

mergeListsI :: (Shifting a, Eq a, Show a) => [[a]] -> [a] -> Result Integer
mergeListsI [] [] = Content 0
mergeListsI [] _  = Error $ ["<A bug in the Matrix.>" :=> []]
mergeListsI [src] lst
  | lst == src = Content 0
  | otherwise = Error $ ["<Invalid pseudo-hash: element mismatch.>" :=> [
      ("Reversing pseudo-hash: {" ++ show lst ++ "}") :=> [],
      ("Using source: {" ++ show src ++ "}") :=> []
    ]]
mergeListsI [l1, l2] res = mergeTwoListsI (l1,l2) res
mergeListsI (l:ls) res = let resWithoutL = filter (`notElem` l) res in liftA2
  (\ x y -> x + mergeListsSpread' ls * mod (y - shift l) (mergeTwoListsSpread (length' l, sum $ map length' ls)))
  (mergeListsI ls resWithoutL)
  (mergeTwoListsI (l, resWithoutL) res)

distribute :: (Eq a) => [[a]] -> [[a]] -> a -> [[a]]
distribute [] _ _ = []
distribute _ [] _ = []
distribute (src:srcRest) (res:resRest) a
  | a `elem` src = (a:res) : resRest
  | otherwise = res : distribute srcRest resRest a

invertMergeLists :: (Eq a) => [[a]] -> [a] -> [[a]]
invertMergeLists srcs [] = [[] | _ <- srcs]
invertMergeLists srcs (a:as) = distribute srcs (invertMergeLists srcs as) a

chooseAndMergeI :: (Shifting a, Eq a, Show a) => [([a], Integer)] -> [a] -> Result Integer
chooseAndMergeI = composeHashingI
  (mapHashingI chooseOrderedI chooseOrderedSpread')
  (product . map chooseOrderedSpread')
  mergeListsI
  mergeListsSpread'
  (invertMergeLists . map fst)

getHashI :: (Shifting a, Eq a, Show a) => [([a], Integer)] -> [a] -> Integer -> Result Integer
getHashI = combineHashingI chooseAndMerge shuffleListI

getHashI' :: (Shifting a, Eq a, Show a) => [([a], Integer)] -> [a] -> Integer -> Result Integer
getHashI' = combineHashingI' chooseAndMergeI shuffleListI'
