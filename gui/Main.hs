module Main where

{-----------------------------------------------------------------------------
    Threepenny

    Hello world!
------------------------------------------------------------------------------}

-- imports
import qualified Graphics.UI.Threepenny      as UI
import           Graphics.UI.Threepenny.Core


-- start a Threepenny server that listens on port 8023 (this is the default)
main = startGUI (defaultConfig { jsPort = Just 8080 }) setup

-- build a user interface whenver a browser connects to the server
setup :: Window -> UI ()
setup window = do
    -- set window title
    return window # set UI.title "pshash gui"
    -- create a button element
    input <- UI.input
    button <- UI.button

