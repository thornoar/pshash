module Inverse where

import Data.List (elemIndex)
import Algorithm
import Error

-- ┌───────────────────┐
-- │ INVERSE FUNCTIONS │
-- └───────────────────┘

mapHashingI :: (Shifting b) => (a -> b -> Result Integer) -> (a -> Integer) -> ([a] -> [b] -> Result Integer)
mapHashingI _ _ [] [] = Content 0
mapHashingI _ _ _ [] = Error $ "<A bug in the Matrix.>" :=> []
mapHashingI _ _ [] _ = Error $ "<A bug in the Matrix.>" :=> []
mapHashingI fI spr (a:as) (b:bs) =
  let curSpr = spr a
      restSpr = product $ map spr as
      curKeyH = fI a b
      restPreKeyH = mapHashingI fI spr as bs -- - shift b
   in case (curKeyH, restPreKeyH) of
        (Error tr1, Error (msg :=> [])) -> Error ("Double trace while mapping pseudo-hash reversal:" :=> [tr1, msg :=> []])
        (Error tr1, Error (_ :=> trss)) -> Error ("Branching trace while mapping pseudo-hash reversal:" :=> (tr1 : trss))
        (Error tr, _) -> Error ("Trace while mapping pseudo-hash reversal:" :=> [tr])
        (_, Error tr) -> Error tr
        (Content curKey, Content nextPreKey) -> Content $ curKey + curSpr * mod (nextPreKey - shift b) restSpr

combineHashingI :: (a -> Integer -> b) -> (b -> c -> Result Integer) -> (a -> c -> Integer -> Result Integer)
combineHashingI f gI a c key1 = gI (f a key1) c

combineHashingI' :: (a -> b -> Result Integer) -> (c -> Integer -> b) -> (a -> c -> Integer -> Result Integer)
combineHashingI' fI gI' a c key2 = fI a (gI' c key2)

composeHashingI :: (Shifting b) => (a -> b -> Result Integer) -> (a -> Integer) -> (b -> c -> Result Integer) -> (b -> Integer) -> (a -> c -> b) -> (a -> c -> Result Integer)
composeHashingI fI sprF gI sprG getB a c =
  let b = getB a c
      keyModH = fI a b
      nextKeyH = gI b c
   in case (keyModH, nextKeyH) of
        (Error tr1, Error tr2) -> Error ("Double trace while composing hashing reversal:" :=> [tr1, tr2])
        (Error tr, _) -> Error ("Trace while composing hashing reversal, in first function:" :=> [tr])
        (_, Error tr) -> Error ("Trace while composing hashing reversal, in second function:" :=> [tr])
        (Content keyMod, Content nextKey) -> Content $ keyMod + sprF a * mod (nextKey - shift b) (sprG b)

chooseOrderedI :: (Shifting a, Eq a, Show a) => ([a], Integer) -> [a] -> Result Integer
chooseOrderedI (_,0) [] = Content 0
chooseOrderedI (src,num) hash
  | num /= length' hash = Error $
      "<Invalid pseudo-hash: length should match source configuration.>" :=>
      [
        ("Reversing pseudo-hash: {" ++ show hash ++ "} with length {" ++ show (length hash) ++ "}") :=> [],
        ("With source: {" ++ show (src,num) ++ "}") :=> []
      ]
chooseOrderedI (src, num) (a:as) =
  let srcLen = length' src
      keyModM = toInteger <$> elemIndex a src
      prevSpread = chooseOrderedSpread (srcLen-1, length' as)
      keyDivH = chooseOrderedI (filter (/= a) src, num-1) as
   in case keyModM of
        Nothing -> Error $
          ("<Invalid pseudo-hash: element {{" ++ show a ++ "}} could not be found in source.>") :=>
          [
            ("Maybe the element {" ++ show a ++ "} is " ++ "{repeated}" ++ " in the pseudo-hash,") :=> [],
            ("Or the pseudo-hash " ++ "{is incompatible}" ++ " with the choice key?") :=> []
          ]
        Just keyMod -> case keyDivH of
          Error tr -> Error tr
          Content keyDiv -> Content $ keyMod + srcLen * mod (keyDiv - shift a) prevSpread
chooseOrderedI (_,_) _ = Error $ "<A bug in the Matrix.>" :=> []

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
  | otherwise = Error $
      "<Invalid pseudo-hash: element mismatch.>" :=>
      [
        ("Reversing pseudo-hash: {" ++ show hash ++ "}") :=> [],
        ("Using source: {" ++ show src ++ "}") :=> []
      ]
mergeTwoListsI (src, []) hash
  | src == hash = Content 0
  | otherwise = Error $
      "<Invalid pseudo-hash: element mismatch.>" :=>
      [
        ("Reversing pseudo-hash: {" ++ show hash ++ "}") :=> [],
        ("Using source: {" ++ show src ++ "}") :=> []
      ]
mergeTwoListsI (e1:rest1, e2:rest2) (m:ms)
  | m == e1 = case mergeTwoListsI (rest1, e2:rest2) ms of
      Error tr -> Error tr
      Content prevKey -> Content $ mod (prevKey - shift m) spr1
  | m == e2 = case mergeTwoListsI (e1:rest1, rest2) ms of
      Error tr -> Error tr
      Content prevKey -> Content $ spr1 + mod (prevKey - shift m) spr2
  | otherwise = Error $
      ("<Invalid pseudo-hash: element {{" ++ show m ++ "}} does not match either source.>") :=>
      [
        ("Reversing pseudo-hash: {" ++ show (m:ms) ++ "}") :=> [],
        ("First source list: {" ++ show (e1:rest1) ++ "}") :=> [],
        ("Second source list: {" ++ show (e2:rest2) ++ "}") :=> [],
        ("The head of the pseudo-hash should be either {" ++ show e1 ++ "} or {" ++ show e2 ++ "}") :=> []
      ]
  where
    spr1 = mergeTwoListsSpread (length' rest1, 1 + length' rest2)
    spr2 = mergeTwoListsSpread (1 + length' rest1, length' rest2)
mergeTwoListsI (_, _) [] = Error $ "<Invalid pseudo-hash: too few elements.>" :=> []

mergeListsI :: (Shifting a, Eq a, Show a) => [[a]] -> [a] -> Result Integer
mergeListsI [] [] = Content 0
mergeListsI [] _  = Error $ "<A bug in the Matrix.>" :=> []
mergeListsI [src] lst
  | lst == src = Content 0
  | otherwise = Error $
    "<Invalid pseudo-hash: element mismatch.>" :=>
      [
        ("Reversing pseudo-hash: {" ++ show lst ++ "}") :=> [],
        ("Current source: {" ++ show src ++ "}") :=> []
      ]
mergeListsI [l1, l2] res = mergeTwoListsI (l1,l2) res
mergeListsI (l:ls) res =
  let curSpr = mergeListsSpread' ls
      nextSpr = mergeTwoListsSpread (length' l, sum $ map length' ls)
      resWithoutL = filter (`notElem` l) res
      keyModH = mergeListsI ls resWithoutL
      nextKeyH = mergeTwoListsI (l, resWithoutL) res
   in case (keyModH, nextKeyH) of
        (Error tr1, Error tr2) -> Error ("Double trace while un-merging lists:" :=> [tr1, tr2])
        (_, Error tr) -> Error ("Trace while un-merging lists:" :=> [tr])
        (Error tr, _) -> Error ("Trace while un-merging lists:" :=> [tr])
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
