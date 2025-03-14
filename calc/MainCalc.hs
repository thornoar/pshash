module Main where

-- import           System.Environment (getArgs)
import qualified UI                 (start, up)

-- | main entry point, also used by the main.js launch script
main :: IO ()
main = UI.up
