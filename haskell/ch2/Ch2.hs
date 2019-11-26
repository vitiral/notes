module Ch2
    ( someFunc
    , sayYo
    , add5
    , tripple
    -- Q1
    , half
    , square
    -- Q2
    , circleAreaRough
    , circleArea
    , area
    , double
    , f
    , letExpressions
    , equivExpressions
    , waxOn
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

sayYo :: String -> String
sayYo input = "Yo " ++ input ++ "!"

add5 :: Int -> Int
add5 x = x + 5

tripple :: Int -> Int
tripple x = x * 3

half :: Double -> Double
half x = x / 2

square :: Double -> Double
square x = x * x

circleAreaRough :: Double -> Double
circleAreaRough r = 3.14 * (square r)

circleArea :: Double -> Double
circleArea r = pi * (square r)

area x = 3.14 * (x * x)

double b = b * 2

x = 7
y = 10
f = x + y


letExpressions = (r1, r2, r3, r4)
    where
        r1 = let x=5 in x
        r2 = let x=5 in x * x
        r3 = let x=5; y=6 in x * y
        r4 = let x=3; y=1000 in x + 3

equivExpressions = (r1, r2, r3, r4, r5)
    where
        r1 = (1 + 1) == 2
        r2 = (10 ^ 2) == 10 + 9 * 10
        r3 = 400 - 37 == (-) 47 400
        r4 = (fromIntegral $ 100 `div` 3) == 100 / 3
        r5 = (2 * 5) + 18 == 2 * (5 + 18)

z2 = 7
x2 = y2 ^ 2
waxOn = x2 * 5
y2 = z2 + 8

