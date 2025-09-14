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

encrypt :: (Eq a, Shifting a) => Integer -> [a] -> [a]
encrypt key = removeId . flip shuffleList key . addId

decrypt :: (Eq a, Shifting a) => Integer -> [a] -> [a]
decrypt key = removeId . flip shuffleListI' key . addId
