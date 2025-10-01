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

-- reverseMap :: (Ord v) => [(k,v)] -> Map v [k]
-- reverseMap [] = M.empty
-- reverseMap ((k,v):rest) =
--   let prev = reverseMap rest
--    in M.insert v (k : M.findWithDefault [] v prev) prev

-- mnemonicList :: [(Char, Char)]
-- mnemonicList = zip ['a'..'z'] $ concat . replicate 3 $ ['0'..'9']

mnemonicList :: [(Char, [Char])]
mnemonicList = [
    ('0', "lps")
  , ('1', "cda")
  , ('2', "yf")
  , ('3', "orn")
  , ('4', "xib")
  , ('5', "msu")
  , ('6', "hw")
  , ('7', "kv")
  , ('8', "qg")
  , ('9', "ze")
  ]

symbols2digits :: Map Char Char
symbols2digits = M.fromList [(k,v) | (v,l) <- mnemonicList, k <- l]

digits2symbols :: Map Char [Char]
digits2symbols = M.fromList mnemonicList

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

-- getPrivateStr :: Integer -> String
-- getPrivateStr 0 = ""
-- getPrivateStr n =
--   let (next, cur) = divMod n 52
--       cur' = fromInteger cur
--    in chr (if cur' < 26 then cur' + 65 else cur' + 71) : getPrivateStr next

-- ┌────────────────────────┐
-- │ CONSTRUCTING MNEMONICS │
-- └────────────────────────┘

newtype Dict = Dict { fromDict :: Map Char Dict }
  deriving (Show, Read)

defaultMnemonicAmount :: Int
defaultMnemonicAmount = 5

printDict :: String -> Dict -> IO ()
printDict prefix (Dict m) =
  sequence_ $ M.mapWithKey
  (\ c d -> putStrLn (prefix ++ show c) >> printDict (prefix ++ "  ") d) m

sortByLetter :: [String] -> Map Char [String]
sortByLetter [] = M.empty
sortByLetter ([]:rest) = M.insert (chr 0) [] $ sortByLetter rest
sortByLetter ((c:str):rest) =
  let prev = sortByLetter rest
   in M.insert c (str : M.findWithDefault [] c prev) prev

list2dict :: [String] -> Dict
list2dict lst = Dict $ M.map list2dict (sortByLetter lst)

constructMnemonics :: Dict -> Dict -> String -> [String]
constructMnemonics _ (Dict curmap) []
  | M.member '\NUL' curmap = [""]
  | otherwise = []
constructMnemonics dct (Dict curmap) (c:rest)
  | M.member '\NUL' curmap =
      constructMnemonics dct (Dict $ M.delete '\NUL' curmap) (c:rest)
      ++ map (' ' :) (constructMnemonics dct dct (c:rest))
  | otherwise =
    let trySymbol :: Char -> [String]
        trySymbol ch = case M.lookup ch curmap of
          Nothing -> []
          Just (Dict mp) -> map (ch :) $ constructMnemonics dct (Dict mp) rest
     in concatMap trySymbol (M.findWithDefault [] c digits2symbols)

getMnemonics :: Int -> String -> IO [String]
getMnemonics num str = do
  dct <- fmap (list2dict . lines) (readFile "./lib/words.txt")
  -- printDict "" dct
  return $ take num $ concatMap (constructMnemonics dct dct) [str, '0':str, '0':'0':str]
