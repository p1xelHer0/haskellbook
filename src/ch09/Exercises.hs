module CH9Excercises where

import Data.Bool
import Data.Char

myHead :: [t] -> t
myHead (x:_) = x

safeHead :: [t] -> Maybe t
safeHead [] = Nothing
safeHead (x:_) = Just x

-- EnumFromTo {{{
eftBool :: Bool -> Bool -> [Bool]
eftBool a b
  | a > b = []
  | a == b = [a]
  | otherwise = a : eftBool (succ a) b

eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd a b
  | a > b = []
  | a == b = [a]
  | otherwise = a : eftOrd (succ a) b

eftInt :: Int -> Int -> [Int]
eftInt a b
  | a > b = []
  | a == b = [a]
  | otherwise = a : eftInt (succ a) b

eftChar :: Char -> Char -> String
eftChar a b
  | a > b = []
  | a == b = [a]
  | otherwise = a : eftChar (succ a) b

-- generally?
eftAll
  :: (Ord a, Enum a)
  => a -> a -> [a]
eftAll a b
  | a > b = []
  | a == b = [a]
  | otherwise = a : eftAll (succ a) b

-- }}}
-- Thy Fearful Symmetry {{{
-- 1.
myWords :: String -> [String]
myWords [] = []
myWords (' ':s) = myWords s
myWords s = takeWhile (/= ' ') s : myWords (dropWhile (/= ' ') s)

-- 2. {{{
firstSen = "Tyger Tyger, burning bright\n"

secondSen = "In the forests of the night\n"

thirdSen = "What immortal hand or eye\n"

fourthSen = "Could frame thy fearful symmetry?"

sentences = firstSen ++ secondSen ++ thirdSen ++ fourthSen

-- putStrLn sentences -- should print
-- Tyger Tyger, burning bright
-- In the forests of the night
-- What immortal hand or eye
-- Could frame thy fearful symmetry?
-- Implement this
myLines :: String -> [String]
myLines [] = []
myLines ('\n':s) = myLines s
myLines s = takeWhile (/= '\n') s : myLines (dropWhile (/= '\n') s)

shouldEqual =
  [ "Tyger Tyger, burning bright"
  , "In the forests of the night"
  , "What immortal hand or eye"
  , "Could frame thy fearful symmetry?"
  ]

main :: IO ()
main = print $ "Are they equal? " ++ show (myLines sentences == shouldEqual)

-- }}}
-- 3. {{{
mySplit :: Char -> String -> [String]
mySplit _ [] = []
mySplit c s@(x:xs) =
  if c == x
    then mySplit c xs
    else takeWhile (/= c) s : mySplit c (dropWhile (/= c) s)

--}}}
-- Comprehend Thy Lists {{{
mySqr =
  [ x ^ 2
  | x <- [1 .. 10] ]

f1 =
  [ x
  | x <- mySqr
  , rem x 2 == 0 -- => [4, 16, 36, 64, 100]
    ]

f2 =
  [ (x, y)
  | x <- mySqr
  , y <- mySqr
  , x < 50
  , y > 50 ]

-- [(1, 64), (1, 81), (1, 100),
--  (4, 64), (4, 81), (4, 100),
--  (16, 64), (16, 81), (16, 100),
--  (36, 64), (36, 81), (36, 100),
f3 =
  take
    5
    [ (x, y)
    | x <- mySqr
    , y <- mySqr
    , x < 50
    , y > 50 ]

-- [(1, 64), (1, 81), (1, 100), (4, 64), (4, 18)]
-- }}}
-- Square Cube {{{
mySqr2 =
  [ x ^ 2
  | x <- [1 .. 5] ]

myCube2 =
  [ y ^ 3
  | y <- [1 .. 5] ]

tupleSqrCube =
  [ (x, y)
  | x <- mySqr2
  , y <- myCube2 ]

tupleSqrCube50 =
  [ (x, y)
  | x <- mySqr2
  , y <- myCube2
  , x < 50
  , y < 50 ]

tSC50Length = length tupleSqrCube50

-- }}}
-- Bottom Madness {{{
-- 1. bottom
-- 2. value
-- 3. bottom
-- 4. value
-- 5. bottom
-- 6. value
-- 7. bottom
-- 8. value
-- 9. bottom
-- 10. bottom
-- }}}
-- Intermission: is it in normal form? {{{
-- 1. NF
-- 2. WHNF
-- 3. NF
-- 4. WHNF
-- 5. NF
-- 6. WHNF
-- 7. neither
-- }}}
-- More Bottoms {{{
-- 1. bottom
-- 2. value, 2
-- 3. bottom
-- 4. returns a list that checks if the current letter is a vowel
-- 5.
-- a) [1, 4, 9, 16, 25, 36, 56, 64, 81, 100]
-- b) [1, 10, 20]
-- c) [15, 15, 15]
-- 6.
mapBool x = map (bool x) [1 .. 10]

-- }}}
-- Filtering {{{
-- 1.
multiplesOfThree = filter (\x -> x `mod` 3 == 0) [1 .. 30]

-- 2.
howManyMultiplesOfThree = length multiplesOfThree

-- 3.
article :: String -> Bool
article "the" = True
article "an" = True
article "a" = True
article _ = False

myFilter :: String -> [String]
myFilter = filter (not . article) . myWords

-- }}}
-- Zipping {{{
-- 1.
myZip :: [a] -> [b] -> [(a, b)]
myZip [] _ = []
myZip _ [] = []
myZip (x:xs) (y:ys) = (x, y) : myZip xs ys

-- 2.
myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith _ [] _ = []
myZipWith _ _ [] = []
myZipWith f (x:xs) (y:ys) = f x y : myZipWith f xs ys

myZipWithZipWith :: [a] -> [b] -> [(a, b)]
myZipWithZipWith = myZipWith (\x y -> (x, y))
-- }}}


-- Chapter Excersies {{{
-- Data.Char {{{
-- 1.
-- isUpper :: Char -> Bool
-- isLower :: Char -> Bool
-- 2.
filterUpperCase :: String -> String
filterUpperCase = filter isUpper
-- 3.
capitalizeFirst :: String -> String
capitalizeFirst (x:xs) = toUpper x : xs
-- 4.
capitalize :: String -> String
capitalize [] = []
capitalize (x:xs) = toUpper x : capitalize xs
-- 5 + 6.
capitalizeHead :: String -> Char
capitalizeHead = toUpper . head
-- }}}
-- }}}
