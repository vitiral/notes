module Main where

import Lib

main :: IO ()
main = do
    someFunc
    print (sayYo "Haskellers")
    print (add5 4)

