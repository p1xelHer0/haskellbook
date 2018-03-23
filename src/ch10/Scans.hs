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

-- 1.
fibs20 :: [Integer]
fibs20 = take 20 fibs

-- 2.
fibsMax100 :: [Integer]
fibsMax100 = takeWhile (100 >) fibs

-- 3.
factorialRec :: Integer -> Integer
factorial 0 = 1
factorialRec n = n * factorialRec (n - 1)

factorialScanl :: Integer -> Integer
factorialScanl 0 = 1
factorialScanl n = last $ scanl (*) 1 [1..n]
--                        foldl (*) 1 [1..n]

