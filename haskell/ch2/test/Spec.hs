module Main where

import Test.Tasty
import Test.Tasty.HUnit

import Lib
import MathLib

main :: IO ()
main = do
    defaultMain (testGroup "Ch2 tests" [sayYoTest, add5Test])

sayYoTest :: TestTree
sayYoTest = testCase "Testing sayYo"
    (assertEqual "Should say Yo to Friend!" "Yo Friend!" (sayYo "Friend"))

add5Test :: TestTree
add5Test = testCase "Testing add5"
    (assertEqual "10 + 5" 15 (add5 10))

