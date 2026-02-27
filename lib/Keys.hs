module Keys where

import Error
import Data.Char (ord, chr)
import Data.List (elemIndex)

-- ┌────────────────────────┐
-- │ READING THE PUBLIC KEY │
-- └────────────────────────┘

getPublicKey' :: String -> Integer
getPublicKey' "" = 0
getPublicKey' (c:cs) = toInteger (ord c) + 128 * getPublicKey' cs

getPublicKey :: String -> Integer
getPublicKey = getPublicKey' . reverse

getPublicStr' :: Integer -> String
getPublicStr' 0 = ""
getPublicStr' key = chr (fromInteger (mod key 128)) : getPublicStr' (div key 128)

getPublicStr :: Integer -> String
getPublicStr = reverse . getPublicStr'

-- ┌──────────────────────┐
-- │ READING PRIVATE KEYS │
-- └──────────────────────┘

commonCombinations :: [[Char]]
commonCombinations = [
    "or","un","el","is","it","us","of","ag","um","yu","in","er","es",
    "ti","re","te","le","ra","li","ri","ne","se","de","co","ro","la",
    "di","ca","ta","ve","he","si","me","pe","ni","lo","ma","mi","to",
    "ce","na","ho","ge","hi","ha","po","pa","no","ci","pi","ke","mo",
    "ba","be","sa","fi","bo","su","so","bi","tu","vi","gi","ru","ku",
    "ga","ko","qu","lu","ki","do","fe","fo","bu","da","we","va","fu",
    "wa","fa","mu","pu","go","wo","gu","du","nu","hu","vo","yi","ze",
    "ye","ju","jo","xi","ka","xe","ja","zi","je"
  ]

readMnemonic :: [[Char]] -> Result [Char]
readMnemonic [] = Content []
readMnemonic (pt:rest) = case elemIndex pt commonCombinations of
  Nothing -> Error $ ("<Unknown mnemonic syllable: \"{{" ++ pt ++ "}}\".>") :=> []
  Just n ->
    let sn = show n
     in fmap ((replicate (2 - length sn) '0' ++ sn) ++) (readMnemonic rest)

getPrivateKeyMnemonic :: [Char] -> Result Integer
getPrivateKeyMnemonic = fmap read . readMnemonic . getPairs . filter (/= ' ')

power :: [Integer] -> Integer
power = foldr (^) 1

getPrivateKeyNum :: String -> Result Integer
getPrivateKeyNum s
  | '+' `elem` s = sum <$> mapM getPrivateKeyNum (splitBy '+' s)
  | '*' `elem` s = product <$> mapM getPrivateKeyNum (splitBy '*' s)
  | '^' `elem` s = power <$> mapM getPrivateKeyNum (splitBy '^' s)
  | otherwise = readResult "integer" s

getPrivateKey :: String -> Result Integer
getPrivateKey s = case getPrivateKeyNum s of
  Content n -> Content n
  Error tr1 -> case getPrivateKeyMnemonic s of
    Content n -> Content n
    Error tr2 -> Error $ "Both arithmetic and mnemonic read methods failed:" :=> [tr1, tr2]

-- ┌────────────────────────┐
-- │ CONSTRUCTING MNEMONICS │
-- └────────────────────────┘

includeSpaces :: [String] -> [String]
includeSpaces [s1, s2, s3] = [s1, s2, s3]
includeSpaces (s1 : s2 : s3 : rest) = s1 : s2 : s3 : " " : includeSpaces rest
includeSpaces strs = strs

getPairs :: [a] -> [[a]]
getPairs (a1:a2:rest) = [a1,a2] : getPairs rest
getPairs [] = []
getPairs [a] = [[a]]

getCombinations :: [Int] -> [String]
getCombinations = foldr (\ n -> ((commonCombinations !! n) :)) []

getMnemonic :: Integer -> String
getMnemonic n =
  let nstr = show n
      nums :: [Int]
      nums = map read $ getPairs $ if mod (length nstr) 2 == 1 then '0':nstr else nstr
   in concat $ includeSpaces (getCombinations nums)
