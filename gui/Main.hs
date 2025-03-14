module Main where

-- import           System.Environment (getArgs)
import qualified UI                 (start)

-- | main entry point, also used by the main.js launch script
main :: IO ()
main = do
  port <- getLine
  UI.start (read port)
