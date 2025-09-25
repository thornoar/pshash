{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE BangPatterns #-}
module Encryption where

import Algorithm (Shifting, shift, shuffleList)
import Data.Word (Word8)
import GHC.Bits (xor)
import qualified Data.ByteString as B (ByteString, unpack, pack, length, concat, packZipWith, map, index)
import System.Random

instance Shifting Int where
  shift = (2^)

defaultSize :: Int
defaultSize = 64

randomGen :: StdGen
randomGen = mkStdGen defaultSize

fpow :: (a -> Integer -> a) -> Integer -> (a -> Integer -> a)
fpow _ 0 = const
fpow f n = \a k -> f (fpow f (n - 1) a k) k

xorbs :: B.ByteString -> B.ByteString -> B.ByteString
xorbs = B.packZipWith xor

processBlock :: B.ByteString -> B.ByteString -> B.ByteString
processBlock perm blk = B.map (B.index blk . fromIntegral) perm

buildStream :: Integer -> B.ByteString -> [B.ByteString]
buildStream 0 _ = []
buildStream n perm =
  processBlock perm (fst $ uniformByteString defaultSize randomGen)
  : buildStream (n-1) perm

procrypt :: B.ByteString -> Integer -> B.ByteString
procrypt plaintext k =
  let !perm = B.pack $ map fromIntegral $ shuffleList [0 .. (defaultSize - 1)] k
      num = toInteger $ 1 + div (B.length plaintext) defaultSize
      stream = B.concat (buildStream num perm)
   in xorbs plaintext stream

correctness :: [Word8] -> Integer -> Bool
correctness msg k = (==) msg $ B.unpack $ procrypt (procrypt (B.pack msg) k) k
