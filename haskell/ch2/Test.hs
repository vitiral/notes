module Main where

import Test.Tasty
import Test.Tasty.HUnit

import Ch2

main :: IO ()
main = do defaultMain $ testGroup "Ch2 tests" tests

waxOnExpected = ((7 + 8) ^ 2) * 5

tests = 
    [ testCase "sayYo Friend"
        $ assertEqual "" "Yo Friend!" (sayYo "Friend")

    , testCase "add5 10"
        $ assertEqual "" 15 $ add5 10

    , testCase "sayYo empty"
        $ assertEqual "Should say Yo to Friend!" "Yo !" (sayYo "")

    , testCase "half 10"
        $ assertEqual "" 5.0 $ half 10.0

    , testCase "square 2"
        $ assertEqual "" 4.0 $ square 2.0

    , testCase "circleAreaRough 2"
        $ assertEqual "" 12.56 $ circleAreaRough 2.0

    , testCase "circleArea 2"
        $ assertEqual "" 12.566370614359172 $ circleArea 2.0

    , testCase "area broken"
        $ assertEqual "" 12.56 $ area 2.0

    , testCase "double"
        $ assertEqual "" 10 $ double 5

    , testCase "f=17"
        $ assertEqual "" 17 f

    , testCase "letExpressions"
        $ assertEqual "" (5, 25, 30, 6) letExpressions

    , testCase "equivExpressions"
        $ assertEqual "" (True, True, False, False, False) equivExpressions

    , testCase "waxOn 1"
        $ assertEqual ""
          ( 10 + waxOn
          , (+10) waxOn
          , (-) 15 waxOn
          , (-) waxOn 15
          ) 
          (10 + waxOnExpected, 10 + waxOnExpected, 15 - waxOnExpected, waxOn - 15)
    ]
