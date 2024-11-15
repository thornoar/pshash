module Test where

import Main

-- ┌────────────────────┐
-- │ INVERSE VALIDATION │
-- └────────────────────┘

isInverseOnRange :: (a -> Integer -> b) -> (a -> b -> Integer) -> a -> [Integer] -> Integer -> IO ()
isInverseOnRange _ _ _ [] _ = putStrLn "| All tests passed. |"
isInverseOnRange f fI a (key:rest) lim =
  if key == fI a (f a key)
  then putStrLn (show key ++ ": Passed. " ++ show (lim - 1) ++ " left.") >> isInverseOnRange f fI a rest (lim - 1)
  else do
    putStrLn ("| Failed on number " ++ show key ++ " with:")
    putStrLn ("|   key = " ++ show key)
    putStrLn ("|   output = " ++ show (fI a (f a key)))

isInverseOnRange2 :: (a -> Integer -> Integer -> b) -> (a -> b -> Integer -> Integer) -> a -> Integer -> [Integer] -> Integer -> IO ()
isInverseOnRange2 f2 f2I a key1 key2s lim =
  let f a' = f2 a' key1
      fI a' b = f2I a' b key1
   in isInverseOnRange f fI a key2s lim

isInverseOnRange2' :: (a -> Integer -> Integer -> b) -> (a -> b -> Integer -> Integer) -> a -> Integer -> [Integer] -> Integer -> IO ()
isInverseOnRange2' f2 f2I a key2 key1s lim =
  let f a' key1 = f2 a' key1 key2
      fI a' b = f2I a' b key2
   in isInverseOnRange f fI a key1s lim

isInverse :: (a -> Integer -> b) -> (a -> Integer) -> (a -> b -> Integer) -> a -> IO ()
isInverse f sprF fI a = isInverseOnRange f fI a [0 .. sprF a - 1] (sprF a)

isInverse2 :: (a -> Integer -> Integer -> b) -> (a -> Integer) -> (a -> b -> Integer -> Integer) -> a -> Integer -> IO ()
isInverse2 f2 sprF f2I a key1 = isInverseOnRange2 f2 f2I a key1 [0 .. sprF a - 1] (sprF a)

isInverse2' :: (a -> Integer -> Integer -> b) -> (a -> Integer) -> (a -> b -> Integer -> Integer) -> a -> Integer -> IO ()
isInverse2' f2 sprF f2I a key2 = isInverseOnRange2' f2 f2I a key2 [0 .. sprF a - 1] (sprF a)
