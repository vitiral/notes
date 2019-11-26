module Main where

import Test.Tasty
import Test.Tasty.HUnit

import Ch3

main :: IO ()
main = do defaultMain $ testGroup "Ch3 tests" tests

tests = 
    [ testCase "sayYo Friend"
        $ assertEqual "" "Yo Friend!" (sayYo "Friend")
    , testCase "3.4 #1"
        $ assertEqual "" 35 subex3_4_1
    , testCase "exercise 1"
        $ assertEqual "" 
          ( [1, 2, 3, 4, 5, 6]
          , [1, 2, 3, 4, 5, 6]
          , "hello world"
          , "hello world"
          , 'o'
          , 'o'
          , "love"
          , "awe"
          )
          ex1
    , let 
        expected =
            ( [6, 12, 18]
            , "rainbow"
            , 10
            , "Jules"
            , [ 2, 3, 5, 6, 8, 9]
            )
      in
        testCase "exercise 2" (assertEqual "" expected ex2)
    ]
