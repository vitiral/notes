module Main where

import Test.Tasty
import Test.Tasty.HUnit
import GHC.Int
import GHC.Word

import Ch4

main :: IO ()
main = do defaultMain $ testGroup "Ch3 tests" tests

tests = 
    [ testCase "sayYo Friend"
        $ assertEqual "" "Yo Friend!" (sayYo "Friend")
    , let
        expected =
            -- Int8
            ( -128, 127
            -- Int16
            , -32768
            -- Word8
            , 0 , 255
            ) 
        result =
            ( minBound :: Int8 , maxBound :: Int8

            , minBound :: Int16

            , minBound :: Word8 , maxBound :: Word8
            )
      in
        testCase "minBound" $ assertEqual "" expected result
    ]
