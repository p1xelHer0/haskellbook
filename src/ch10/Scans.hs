module Scans where

fibonacci
  :: Integral a
  => a -> a
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci x = fibonacci (x - 1) + fibonacci (2 - x)

fibs :: [Integer]
fibs = 1 : scanl (+) 1 fibs

fibsN :: Int -> Integer
fibsN x = fibs !! x

-- 1. Modify your fibs function to only return the  rst 20 Fi- bonacci numbers
fibs20 :: [Integer]
fibs20 = take 20 fibs

-- 2. Modify fibs to return the Fibonacci numbers that are less than 100
fibsMax100 :: [Integer]
fibsMax100 = takeWhile (100 >) fibs

-- 3. Try to write the factorial function from Recursion as a scan.
--    You’ll want scanl again, and your start value will be 1.
--    Warning: this will also generate an infinite list,
--    so you may want to pass it through a take function or similar
factorialRec :: Integer -> Integer
factorial 0 = 1
factorialRec n = n * factorialRec (n - 1)

factorialScanl :: Integer -> Integer
factorialScanl 0 = 1
factorialScanl n = last $ scanl (*) 1 [1..n]
--                        foldl (*) 1 [1..n]

