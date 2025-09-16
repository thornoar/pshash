{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE BangPatterns #-}
module Encryption where

import Algorithm (Shifting, shift, shuffleList)
import Inverse (shuffleListI')
import Data.Word (Word8)

instance Shifting a => Shifting (Integer, a) where
  shift (_, a) = shift a

instance Shifting Word8 where
  shift = toInteger

defaultIterations :: Integer
defaultIterations = 10

defaultSize :: Integer
defaultSize = 30

partition :: Integer -> [a] -> [a] -> [[a]]
partition _ !acc [] = [acc]
partition 0 !acc lst = reverse acc : partition defaultSize [] lst
partition n !acc (a:rest) = partition (n - 1) (a : acc) rest

fpow :: (a -> Integer -> a) -> Integer -> (a -> Integer -> a)
fpow f 1 = f
fpow f n = \a k -> f (fpow f (n - 1) a k) k

addId :: [a] -> [(Integer, a)]
addId = zip [0..]

removeId :: [(Integer, a)] -> [a]
removeId = map snd

encrypt :: (Eq a, Shifting a) => [a] -> Integer -> [a]
encrypt !plt k = concatMap (\l -> fpow shuffleList defaultIterations l k) (partition defaultSize [] plt)

decrypt :: (Eq a, Shifting a) => [a] -> Integer -> [a]
decrypt !cpt k = concatMap (\l -> fpow shuffleListI' defaultIterations l k) (partition defaultSize [] cpt)

correctness :: [Word8] -> Integer -> Bool
correctness str k = str == removeId (decrypt (encrypt (addId str) k) k)
