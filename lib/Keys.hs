module Keys where

import Error
import Data.Char (ord, chr)
import Data.Map (Map)
import qualified Data.Map as M

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

reverseMap :: (Ord v) => [(k,v)] -> Map v [k]
reverseMap [] = M.empty
reverseMap ((k,v):rest) =
  let prev = reverseMap rest
   in if M.member v prev then M.update (Just . (k :)) v prev else M.insert v [k] prev

mnemonicList :: [(Char, Char)]
mnemonicList = zip ['a'..'z'] $ concat . replicate 3 $ ['0'..'9']

symbols2digits :: Map Char Char
symbols2digits = M.fromList mnemonicList

digits2symbols :: Map Char [Char]
digits2symbols = reverseMap mnemonicList

convertToDigits :: [Char] -> Result [Char]
convertToDigits [] = Content []
convertToDigits (c:rest) = case M.lookup c symbols2digits of
  Nothing -> Error $ ("<Invalid character: {{" ++ show c ++ "}}.>") :=> []
  Just d -> fmap (d:) (convertToDigits rest)

getPrivateKeyMnemonic :: [Char] -> Result Integer
getPrivateKeyMnemonic = fmap read . convertToDigits . filter (/= ' ')

-- getPrivateKeyAlpha :: String -> Result Integer
-- getPrivateKeyAlpha [] = Content 0
-- getPrivateKeyAlpha (c:rest)
--   | 65 <= n && n <= 90 = (n - 65 +) . (52 *) <$> getPrivateKeyAlpha rest
--   | 97 <= n && n <= 122 = (n - 71 +) . (52 *) <$> getPrivateKeyAlpha rest
--   | otherwise = Error $ "<Private key string must consist of English alphabets only.>" :=> [
--       ("The character {" ++ show c ++ "} is invalid.") :=> []
--     ]
--   where n = toInteger (ord c)

getPrivateKeyNum :: String -> Result Integer
getPrivateKeyNum s = liftA2 (^) base pow where
  (baseStr, powStr) = splitBy '^' s
  base :: Result Integer
  base = readResult "base in private key" baseStr
  pow :: Result Integer
  pow = if null powStr then Content 1 else case readResult "exponent in private key" powStr of
    Error tr -> Error tr
    Content n ->
      if n < 0
      then Error $ ("<Cannot have negative exponent in private key: {{" ++ powStr ++ "}}.>") :=> []
      else Content n

getPrivateKey :: String -> Result Integer
getPrivateKey s = case getPrivateKeyNum s of
  Content n -> Content n
  Error tr1 -> case getPrivateKeyMnemonic s of
    Content n -> Content n
    Error tr2 -> Error $ "Both numeric and mnemonic read methods failed:" :=> [tr1, tr2]

getPrivateStr :: Integer -> String
getPrivateStr 0 = ""
getPrivateStr n =
  let (next, cur) = divMod n 52
      cur' = fromInteger cur
   in chr (if cur' < 26 then cur' + 65 else cur' + 71) : getPrivateStr next
