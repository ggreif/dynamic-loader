module Upper (plugin) where

import Char (toUpper)

-- a plugin should export a function "plugin" which tells
-- the web browser which extension to handle as well as which 
-- function to run with each page content

plugin = ("upp", handleUpper)

handleUpper :: String -> IO String
handleUpper str = return (map toUpper str)
