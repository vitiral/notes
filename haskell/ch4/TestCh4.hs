module Main where

import Test.Tasty
import Test.Tasty.HUnit

import Ch4

main :: IO ()
main = do defaultMain $ testGroup "Ch3 tests" tests

tests = 
    [ testCase "sayYo Friend"
        $ assertEqual "" "Yo Friend!" (sayYo "Friend")
    ]
