module Error where

import Control.Monad (liftM)
import Text.Read (readMaybe)
import Data.Char (toUpper)
import System.IO (stderr, hPutStrLn)

-- ┌────────────────┐
-- │ ERROR HANDLING │
-- └────────────────┘

data Trace = String :=> [Trace] deriving (Read, Show, Eq)

data Result a = Content a | Error Trace deriving (Read, Show, Eq)

splitBy :: (Eq a) => a -> [a] -> [[a]]
splitBy _ [] = [[]]
splitBy a (a':rest)
  | a == a' = [] : splitBy a rest
  | otherwise = case splitBy a rest of
      [] -> [[a']]
      (str:strs) -> (a':str) : strs

addTrace :: String -> Result a -> Result a
addTrace _ (Content a) = Content a
addTrace msg (Error tr) = Error (msg :=> [tr])

handleWith :: (Monad m) => (a -> m ()) -> Result a -> m (Result ())
handleWith _ (Error tr) = return (Error tr)
handleWith f (Content a) = f a >> return (Content ())

handleWithMsg :: (Monad m) => String -> Result a -> (a -> m b) -> m (Result b)
handleWithMsg msg (Error tr) _ = return $ Error (msg :=> [tr])
handleWithMsg _ (Content a) f = f a >>= return . Content

handleWithMsg' :: (Monad m) => String -> Result a -> (a -> m (Result b)) -> m (Result b)
handleWithMsg' msg (Error tr) _ = return $ Error (msg :=> [tr])
handleWithMsg' _ (Content a) f = f a

mergeTrace :: String -> (a -> b -> c) -> Result a -> Result b -> Result c
mergeTrace msg f ma mb = case (ma, mb) of
  (Content a, Content b) -> Content (f a b)
  (Error tr, Content _) -> Error (msg :=> [tr])
  (Content _, Error tr) -> Error (msg :=> [tr])
  (Error tr1, Error tr2) -> Error (msg :=> [tr1, tr2])

mergeTraceM :: (Monad m) => String -> Result a -> Result b -> (a -> b -> m c) -> m (Result c)
mergeTraceM msg ma mb f = case (ma, mb) of
  (Content a, Content b) -> f a b >>= return . Content
  (Error tr, Content _) -> return $ Error (msg :=> [tr])
  (Content _, Error tr) -> return $ Error (msg :=> [tr])
  (Error tr1, Error tr2) -> return $ Error (msg :=> [tr1, tr2])

raiseM :: (Monad m) => (a -> m (Result b)) -> (Result a -> m (Result b))
raiseM f (Content a) = f a
raiseM _ (Error tr) = return (Error tr)

instance Functor Result where
  fmap = liftM
instance Applicative Result where
  pure = Content
  mf <*> ma = mergeTrace "@<*>" id mf ma
instance Monad Result where
  return = pure
  mval >>= f = case mval of
    Error tr -> Error tr
    Content a -> f a

fmap2 :: (Monad m) => (a -> b -> c) -> (m a -> b -> m c)
fmap2 f ma b = fmap (`f` b) ma

raise2 :: (Monad m) => (a -> b -> m c) -> (m a -> b -> m c)
raise2 f ma b = ma >>= (`f` b)

raise2' :: (Monad m) => (a -> b -> m c) -> (a -> m b -> m c)
raise2' f a mb = mb >>= f a

readResult :: (Read a) => String -> String -> Result a
readResult msg str = case readMaybe str of
  Nothing -> Error $ ("<Failed to read \"{{" ++ str ++ "}}\" as {{" ++ msg ++ "}}.>") :=> []
  Just a -> Content a

printTraceList :: [Bool] -> [Trace] -> IO ()
printTraceList _ [] = return ()
printTraceList places [tr] = printTrace places [False,False] tr
printTraceList places (tr:rest) = printTrace places [True, False] tr >> printTraceList places rest

printTrace :: [Bool] -> [Bool] -> Trace -> IO ()
printTrace places pass (msg :=> lst) = do
  let prefix = map (\b -> if b then '|' else ' ') places ++ "|_"
  hPutStrLn stderr $ prefix ++ msg
  printTraceList (places ++ pass) lst

formatWithColor :: String -> String
formatWithColor [] = []
formatWithColor ('<':rest) = "\ESC[31m" ++ formatWithColor rest
formatWithColor ('>':rest) = "\ESC[0m" ++ formatWithColor rest
formatWithColor ('{':'{':rest) = "\ESC[33m" ++ formatWithColor rest
formatWithColor ('}':'}':rest) = "\ESC[31m" ++ formatWithColor rest
formatWithColor ('{':rest) = "\ESC[33m" ++ formatWithColor rest
formatWithColor ('}':rest) = "\ESC[0m" ++ formatWithColor rest
formatWithColor (c:rest) = c : formatWithColor rest

formatWithoutColor :: String -> Bool -> String
formatWithoutColor [] _ = []
formatWithoutColor ('<':rest) _ = formatWithoutColor rest True
formatWithoutColor ('>':rest) _ = formatWithoutColor rest False
formatWithoutColor ('{':'{':rest) _ = '*' : formatWithoutColor rest False
formatWithoutColor ('}':'}':rest) _ = '*' : formatWithoutColor rest True
formatWithoutColor ('{':rest) upper = '*' : formatWithoutColor rest upper
formatWithoutColor ('}':rest) upper = '*' : formatWithoutColor rest upper
formatWithoutColor (c:rest) True = toUpper c : formatWithoutColor rest True
formatWithoutColor (c:rest) False = c : formatWithoutColor rest False

formatTrace :: Bool -> Trace -> Trace
formatTrace True (msg :=> trs) = formatWithColor msg :=> map (formatTrace True) trs
formatTrace False (msg :=> trs) = formatWithoutColor msg False :=> map (formatTrace False) trs
