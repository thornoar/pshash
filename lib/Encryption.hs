{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE BangPatterns #-}
module Encryption where

import Algorithm (Shifting, shift, shuffleList)
import Data.Word (Word8)
import GHC.Bits (xor)
import qualified Data.ByteString as B (ByteString, unpack, pack, length, concat, packZipWith, map, index, replicate)

instance Shifting Int where
  shift = (2^)

defaultSize :: Int
defaultSize = 64

defaultSeed :: B.ByteString
defaultSeed = B.pack $ take defaultSize [
    126,187,163,5,14,90,101,222,155,52,30,228,191,98,232,29,250,
    39,164,35,23,22,248,238,203,31,190,213,30,132,81,242,110,218,
    195,146,43,115,118,33,119,226,211,210,159,255,140,10,216,110,
    70,241,51,59,70,25,53,95,109,133,87,52,141,121,119,132,131,186,
    62,129,43,154,91,42,252,133,249,93,129,167,58,19,111,84,96,126,
    72,190,139,225,68,23,65,141,14,48,127,27,178,101,156,231,200,53,
    92,250,207,149,35,114,155,162,246,186,246,132,192,76,169,236,87,
    181,76,23,153,111,162,86
  ]

fpow :: (a -> Integer -> a) -> Integer -> (a -> Integer -> a)
fpow _ 0 = const
fpow f n = \a k -> f (fpow f (n - 1) a k) k

xorbs :: B.ByteString -> B.ByteString -> B.ByteString
xorbs = B.packZipWith xor

processBlock :: B.ByteString -> B.ByteString -> B.ByteString
processBlock !perm !blk = B.map (B.index blk . fromIntegral) perm

buildStream :: Integer -> B.ByteString -> B.ByteString -> [B.ByteString]
buildStream 0 _ _ = []
buildStream n perm prev =
  let cur = processBlock perm (xorbs prev defaultSeed) in cur : buildStream (n-1) perm cur

procrypt :: B.ByteString -> Integer -> B.ByteString
procrypt plaintext k =
  let !perm = B.pack $ map fromIntegral $ shuffleList [0 .. (defaultSize - 1)] k
      num = toInteger $ 1 + div (B.length plaintext) defaultSize
      stream = B.concat (buildStream num perm (B.replicate defaultSize 0))
   in xorbs plaintext stream

correctness :: [Word8] -> Integer -> Bool
correctness msg k = (==) msg $ B.unpack $ procrypt (procrypt (B.pack msg) k) k
