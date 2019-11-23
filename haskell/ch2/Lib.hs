module Lib
    ( someFunc
    , sayYo
    , add5
    , tripple
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

sayYo :: String -> String
sayYo input = "Yo " ++ input ++ "!"

add5 :: Int -> Int
add5 x = x + 5

tripple :: Int -> Int
tripple x = x * 3
