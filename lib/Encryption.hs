{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Encryption where

import Algorithm (shuffleList, Shifting, shift)
import Inverse (shuffleListI')

instance (Shifting a) => Shifting (Integer, a) where
  shift (_, a) = shift a

defaultIterations :: Integer
defaultIterations = 20

fpow :: (a -> Integer -> a) -> Integer -> (a -> Integer -> a)
fpow f 1 = f
fpow f n = \a k -> f (fpow f (n - 1) a k) k

addId :: [a] -> [(Integer, a)]
addId = zip [0..]

removeId :: [(Integer, a)] -> [a]
removeId = map snd

encrypt :: (Eq a, Shifting a) => [a] -> Integer -> [a]
encrypt plt key = removeId (fpow shuffleList defaultIterations (addId plt) key)

decrypt :: (Eq a, Shifting a) => [a] -> Integer -> [a]
decrypt cpt key = removeId (fpow shuffleListI' defaultIterations (addId cpt) key)
