module Ch08.Recursion
  ( digitToWord
  , digits
  , wordNumber
  ) where

import Data.List (intersperse, intercalate)

fibonacci
  :: Integral a
  => a -> a
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci x = fibonacci (x - 1) + fibonacci (x - 2)

--  1.
dividedBy
  :: Integral a
  => a -> a -> (a, a)
dividedBy num denom = go num denom 0
  where
    go n d count
      | n < d = (count, n)
      | otherwise = go (n - d) d (count + 1)

-- -> dividedBy 15 2 = go 15 2 0
-- ->   where
-- ->     go 15 2 0
-- ->     go 13 2 1
-- ->     go 11 2 2
-- ->     go 9  2 3
-- ->     go 7  2 4
-- ->     go 5  2 5
-- ->     go 3  2 6
-- ->     go 1  2 7
-- ->   (7, 1)
-- -> dividedBy 15 2 = (7, 1)
-- 2.
sumNumbers
  :: (Eq a, Num a)
  => a -> a
sumNumbers 0 = 0
sumNumbers n = n + sumNumbers (n - 1)

-- 3.
recursiveMultiply
  :: (Integral a)
  => a -> a -> a
recursiveMultiply _ 0 = 0
recursiveMultiply n1 n2 = n1 + recursiveMultiply n1 (n2 - 1)

-- Fixing dividedBy
data DividedResult
  = Result Integer
  | DividedByZero
  deriving (Show)

-- todo fix
dividedByFixed :: Integer -> Integer -> DividedResult
dividedByFixed num denom = go num denom 0
  where
    go n d count
      | d == 0 = DividedByZero
      | n < d = Result count
      | otherwise = go (n - d) d (count + 1)

-- McCarthy 91 function
mc91
  :: (Ord t, Num t)
  => t -> t
mc91 n
  | n > 100 = n - 10
  | otherwise = mc91 (mc91 (n + 11))

-- Number into words
digitToWord :: Int -> String
digitToWord n
  | n == 0 = "zero"
  | n == 1 = "one"
  | n == 2 = "two"
  | n == 3 = "three"
  | n == 4 = "four"
  | n == 5 = "five"
  | n == 6 = "six"
  | n == 7 = "seven"
  | n == 8 = "eight"
  | n == 9 = "nine"

digits :: Int -> [Int]
digits 0 = []
digits x = digits (x `div` 10) ++ [x `mod` 10]

wordNumber :: Int -> String
wordNumber n = concat (intersperse "-" (map digitToWord (digits n)))
-- why not: wordNumber n = intercalate "-" (map digitToWord (digits n))
