module Main where

import Test.Tasty
import Test.Tasty.HUnit

import Lib

main :: IO ()
main = do
    defaultMain (testGroup "Ch2 tests"  tests)

tests = 
    [ testCase "sayYo Friend"
        $ assertEqual "" "Yo Friend!" (sayYo "Friend")

    , testCase "add5 10"
        $ assertEqual "" 15 $ add5 10

    , testCase "half 10"
        $ assertEqual "" 5.0 $ half 10.0

    , testCase "sayYo empty"
        $ assertEqual "Should say Yo to Friend!" "Yo !" (sayYo "")
    ]
