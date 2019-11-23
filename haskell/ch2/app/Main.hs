module Main where

import Lib
import MathLib

main :: IO ()
main = do
    someFunc
    print (sayYo "Haskellers")
    print (add5 4)

