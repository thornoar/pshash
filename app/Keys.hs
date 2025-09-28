module Keys where

import Error
import Data.Char (ord, chr)

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

getPrivateKeyAlpha :: String -> Result Integer
getPrivateKeyAlpha [] = Content 0
getPrivateKeyAlpha (c:rest)
  | 65 <= n && n <= 90 = (n - 65 +) . (52 *) <$> getPrivateKeyAlpha rest
  | 97 <= n && n <= 122 = (n - 71 +) . (52 *) <$> getPrivateKeyAlpha rest
  | otherwise = Error $ "<Private key string must consist of English alphabets only.>" :=> [
      ("The character {" ++ show c ++ "} is invalid.") :=> []
    ]
  where n = toInteger (ord c)

getPrivateStr :: Integer -> String
getPrivateStr 0 = ""
getPrivateStr n =
  let (next, cur) = divMod n 52
      cur' = fromInteger cur
   in chr (if cur' < 26 then cur' + 65 else cur' + 71) : getPrivateStr next

getPrivateKeyPow :: String -> Result Integer
getPrivateKeyPow s = liftA2 (^) base pow where
  (baseStr, powStr) = splitBy '-' s
  base :: Result Integer
  base = readResult "base in private key" baseStr
  pow :: Result Integer
  pow = if null powStr then Content 1 else case readResult "exponent in private key" powStr of
    Error tr -> Error tr
    Content n ->
      if n < 0
      then Error $ ("<Cannot have negative exponent in private key: {{" ++ powStr ++ "}}.>") :=> []
      else Content n
