{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Encryption where

import Algorithm (shuffleList, Shifting, shift)
import Inverse (shuffleListI')

instance (Shifting a) => Shifting (Integer, a) where
  shift (_, a) = shift a

addId :: [a] -> [(Integer, a)]
addId = zip [0..]

removeId :: [(Integer, a)] -> [a]
removeId = map snd

encrypt :: (Eq a, Shifting a) => [a] -> Integer -> [a]
encrypt plt key = removeId (shuffleList (addId plt) key)

decrypt :: (Eq a, Shifting a) => [a] -> Integer -> [a]
decrypt cpt key = removeId (shuffleListI' (addId cpt) key)
