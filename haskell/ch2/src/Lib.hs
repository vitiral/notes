module Lib
    ( someFunc
    , sayYo
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

sayYo :: String -> String
sayYo input = "Yo " ++ input ++ "!"
