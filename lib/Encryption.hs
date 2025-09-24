{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Encryption where

import Algorithm (Shifting, shift, shuffleList)
import Inverse (shuffleListI')
import Data.Word (Word8)
import GHC.Bits (xor)
import qualified Data.ByteString as BS (ByteString, unpack, pack, append, replicate, null, splitAt, length, concat, take, head, empty)
import System.Random
import Data.Char (chr, ord)

instance Shifting a => Shifting (Integer, a) where
  shift (_, a) = shift a

instance Shifting Word8 where
  shift = toInteger

defaultSize :: Int
defaultSize = 32

defaultSeed :: BS.ByteString
defaultSeed = BS.pack [129,187,110,190,234,122,222,99,23,234,139,233,49,173,200,49,251,58,217,229,86,6,228,245,157,12,0,41,234,252,36,14] 

randomGen :: StdGen
randomGen = mkStdGen defaultSize

fpow :: (a -> Integer -> a) -> Integer -> (a -> Integer -> a)
fpow _ 0 = const
fpow f n = \a k -> f (fpow f (n - 1) a k) k

addId :: [a] -> [(Integer, a)]
addId = zip [0..]

removeId :: [(Integer, a)] -> [a]
removeId = map snd

encryptBlock :: BS.ByteString -> BS.ByteString -> Integer -> BS.ByteString
encryptBlock prev blk k =
  (BS.pack . removeId)
  (shuffleList (addId $ zipWith xor (BS.unpack prev) (BS.unpack blk)) k)

decryptBlock :: BS.ByteString -> BS.ByteString -> Integer -> BS.ByteString
decryptBlock prev blk k =
  (BS.pack . zipWith xor (BS.unpack prev) . removeId)
  (shuffleListI' (addId $ BS.unpack blk) k)

encryptChain :: BS.ByteString -> Bool -> BS.ByteString -> Integer -> Integer -> [BS.ByteString]
encryptChain seed padded ptxt k1 k2
  | BS.null ptxt = []
  | otherwise =
      let (blk, rest, pdd) = case BS.splitAt defaultSize ptxt of
            (str1, str2) | not padded && BS.null str2 ->
              let amt = defaultSize - BS.length str1
               in (BS.append str1 (BS.replicate amt (fromIntegral amt)), BS.replicate defaultSize (fromIntegral amt), True)
            (str1, str2) -> (str1, str2, False)
          cblk = encryptBlock seed blk k1
       in cblk : encryptChain cblk pdd rest k2 k1

decryptChain :: BS.ByteString -> BS.ByteString -> Integer -> Integer -> [BS.ByteString]
decryptChain seed ctxt k1 k2
  | BS.null ctxt = []
  | otherwise =
      let (blk, rest) = BS.splitAt defaultSize ctxt
          rawpblk = decryptBlock seed blk k1
          rawnxt = decryptChain blk rest k2 k1
          (pblk, nxt) = case rawnxt of
            [] -> (rawpblk, [])
            [sth] | BS.null sth -> (rawpblk, [BS.empty])
                  | otherwise -> (BS.take (defaultSize - fromIntegral (BS.head sth)) rawpblk, [BS.empty])
            _ -> (rawpblk, rawnxt)
       in pblk : nxt

encrypt :: BS.ByteString -> Integer -> Integer -> BS.ByteString
encrypt plaintext k1 k2 = BS.concat $ encryptChain defaultSeed False plaintext k1 k2

decrypt :: BS.ByteString -> Integer -> Integer -> BS.ByteString
decrypt ciphertext k1 k2 = BS.concat $ decryptChain defaultSeed ciphertext k1 k2

encryptStr :: String -> Integer -> Integer -> String
encryptStr str k1 k2 = map ((chr :: Int -> Char) . fromIntegral) $ BS.unpack $ encrypt (BS.pack $ map (fromIntegral . (ord :: Char -> Int)) str) k1 k2

decryptStr :: String -> Integer -> Integer -> String
decryptStr str k1 k2 = map ((chr :: Int -> Char) . fromIntegral) $ BS.unpack $ decrypt (BS.pack $ map (fromIntegral . (ord :: Char -> Int)) str) k1 k2

encryptWords :: [Word8] -> Integer -> Integer -> [Word8]
encryptWords str k1 k2 = map fromIntegral $ BS.unpack $ encrypt (BS.pack $ map fromIntegral str) k1 k2

decryptWords :: [Word8] -> Integer -> Integer -> [Word8]
decryptWords str k1 k2 = map fromIntegral $ BS.unpack $ decrypt (BS.pack $ map fromIntegral str) k1 k2

correctness :: [Word8] -> Integer -> Integer -> Bool
correctness msg k1 k2 = (==) msg $ BS.unpack $ decrypt (encrypt (BS.pack msg) k1 k2) k1 k2
