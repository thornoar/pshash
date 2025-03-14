module Main where

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import System.Environment (getArgs)


main :: IO ()
main = do
  [port] <- getArgs
  startGUI (defaultConfig { jsPort = Just (read port) }) setup

setup :: Window -> UI ()
setup window = do
    -- set window title
    return window # set UI.title "Hello World!"
    -- create a button element
    button <- UI.button # set UI.text "Click me!"
    -- attach button to the HTML body, so that it is displayed
    getBody window #+ [element button]
    -- register an event handler for clicking the button
    on UI.click button $ \_ -> do
        element button # set UI.text "I have been clicked!"
