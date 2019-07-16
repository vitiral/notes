doubleMe x = x + x

doubleUs x y = doubleMe x + doubleMe y

doubleSmallNumber x = if x > 100
    then x
    else x*2


oneToTen = [1..10]


-- |return elements that are dividible by 3
--
-- this uses a list comprehension, which follows
-- the same syntax as used in mathematics set-theory
--
-- S = { 2 * x | x E N, x <= 10 }  (where E is "exists in")
divisibleBy3 xs = [x | x <- xs, x `mod` 3 == 0]


removeNonUppercase :: String -> String
removeNonUppercase st = [c | c <- st, c `elem` ['A'..'Z']]


addThree :: Int -> Int -> Int -> Int
addThree x y z = x + y + z


eq' :: (Eq a) => a -> a -> Bool
eq' x y = (x == y)


lucky :: (Show a, Integral a) => a -> String
lucky 7 = "LUCKY NUMBER SEVEN!"
lucky x = "Sorry " ++ show x ++ " isn't lucky"


factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial (n - 1)
