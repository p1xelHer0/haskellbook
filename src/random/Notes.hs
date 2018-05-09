module Notes where

notes :: IO ()
notes = print (palindrome testString)

testString = "blahalb"

testNumber = -7

fifthChar :: String -> Char
fifthChar s = s !! 4

lastWord :: String -> String
lastWord = drop 9

thirdChar :: String -> Char
thirdChar s = s !! 2

letterIndex :: Int -> Char
letterIndex x = testString !! (x - 1)

rvrs :: String -> String
rvrs s = drop 9 s ++ take 4 (drop 5 s) ++ take 5 s

palindrome
  :: (Eq a)
  => [a] -> Bool
palindrome x = x == reverse x

myAbs :: Integer -> Integer
myAbs x =
  if isPositive x
    then x
    else x * (-1)
  where
    isPositive x = x > -1

f :: (a, b) -> (c, d) -> ((b, d), (a, c))
f a b = ((snd a, snd b), (fst a, fst b))

x = (+)

f2 xs = w `x` 1
  where
    w = length xs

foopa = \x -> x

soopa = \x -> head x

doopa (a, b) = a

fs [] b = b
fs (x:xs) b = x `seq` fs xs b
-- head/tail
-- drop/take
-- the arrow, (->), is the type constructor for functions in Haskell.
-- Prelude> :type length
-- length :: [a] -> Int
-- types
-- Num -> Integral -> Int, Integer
-- Num -> Fractional -> Double, Float, Scientific
