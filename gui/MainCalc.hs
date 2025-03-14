module Main where

-- import           System.Environment (getArgs)
import qualified UI                 (start)

-- | main entry point, also used by the main.js launch script
main :: IO ()
main = UI.start 8080
