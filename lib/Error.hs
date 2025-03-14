module Error where

import Control.Monad (liftM)
import Text.Read (readMaybe)

-- ┌────────────────┐
-- │ ERROR HANDLING │
-- └────────────────┘

data Trace = String :=> [Trace] deriving (Read, Show)

data Result a = Content a | Error Trace deriving (Read, Show)

liftH2 :: String -> String -> (a -> b -> c) -> (Result a -> Result b -> Result c)
liftH2 _ _ f (Content a) (Content b) = Content (f a b)
liftH2 msg1 msg2 _ (Error tr1) (Error tr2) = Error $ "Double trace:" :=>
  [
    msg1 :=> [tr1],
    msg2 :=> [tr2]
  ]
liftH2 msg1 _ _ (Error tr) _ = Error (msg1 :=> [tr])
liftH2 _ msg2 _ _ (Error tr) = Error (msg2 :=> [tr])

raiseH' :: (Monad m) => (a -> m (Result b)) -> (Result a -> m (Result b))
raiseH' f (Content a) = f a
raiseH' _ (Error tr) = return (Error tr)

fmapE :: (Trace -> Trace) -> Result a -> Result a
fmapE _ (Content a) = Content a
fmapE f (Error tr) = Error (f tr)

instance Functor Result where
  fmap = liftM
instance Applicative Result where
  pure = Content
  mf <*> ma = case mf of
    Error tr -> case ma of
      Error tr' -> Error ("Double trace:" :=> [tr, tr'])
      Content _ -> Error tr
    Content f -> fmap f ma
instance Monad Result where
  return = pure
  mval >>= f = case mval of
    Error tr -> Error tr
    Content a -> f a

fmap2 :: (Monad m) => (a -> b -> c) -> (m a -> b -> m c)
fmap2 f ma b = fmap (`f` b) ma

raise :: (Monad m) => (a -> m b) -> (m a -> m b)
raise f ma = ma >>= f

raise2 :: (Monad m) => (a -> b -> m c) -> (m a -> b -> m c)
raise2 f ma b = ma >>= (`f` b)

raise2' :: (Monad m) => (a -> b -> m c) -> (a -> m b -> m c)
raise2' f a mb = mb >>= f a

readResult :: (Read a) => String -> String -> Result a
readResult msg str = case readMaybe str of
  Nothing -> Error $ ("<Failed to read {{" ++ str ++ "}} as {{" ++ msg ++ "}}.>") :=> []
  Just a -> Content a

addTrace :: String -> Result a -> Result a
addTrace _ (Content a) = Content a
addTrace msg (Error tr) = Error (msg :=> [tr])
