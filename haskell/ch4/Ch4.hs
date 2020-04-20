module Ch4
    ( sayYo
    ) where

sayYo :: String -> String
sayYo input = "Yo " ++ input ++ "!"

data Mood =
    Blah
    | Woot
    deriving
        Show


changeMood :: Mood -> Mood
changeMood Blah = Woot
changeMood _ = Blah
