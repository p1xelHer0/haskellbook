module Ch14.UsingQuickCheck where

import Data.List (sort)
import Test.QuickCheck

tests :: IO ()
tests = do
  quickCheck prop_halfLessThanWhole
  quickCheck prop_sortedList
  quickCheck prop_plusAssociative
  quickCheck prop_plusCommutative
  quickCheck prop_multiplucationAssociative
  quickCheck prop_multiplucationCommutative
  quickCheck prop_quotRem
  quickCheck prop_divMod
  quickCheck prop_caret

-- 1.
half :: Float -> Float
half x = x / 2

halfIdentity :: Float -> Float
halfIdentity = (* 2) . half

prop_halfLessThanWhole :: Float -> Bool
prop_halfLessThanWhole x = (halfIdentity . half) x == half x

-- 2.
-- for any list you apply sort to
-- this property should hold
listOrdered
  :: (Ord a)
  => [a] -> Bool
listOrdered xs = snd $ foldr go (Nothing, True) xs
  where
    go _ status@(_, False) = status
    go y (Nothing, t) = (Just y, t)
    go y (Just x, t) = (Just y, x >= y)

prop_sortedList :: [Int] -> Bool
prop_sortedList x = listOrdered (sort x)

-- 3.
prop_plusAssociative :: Int -> Int -> Int -> Bool
prop_plusAssociative x y z = x + (y + z) == (x + y) + z

prop_plusCommutative :: Int -> Int -> Bool
prop_plusCommutative x y = x + y == y + x

-- 4.
prop_multiplucationAssociative :: Int -> Int -> Int -> Bool
prop_multiplucationAssociative x y z = x * (y * z) == (x * y) * z

prop_multiplucationCommutative :: Int -> Int -> Bool
prop_multiplucationCommutative x y = x * y == y * x

-- 5.
prop_quotRem :: Int -> Int -> Bool
prop_quotRem x y = (quot x y) * y + (rem x y) == x

prop_divMod :: Int -> Int -> Bool
prop_divMod x y = (div x y) * y + (mod x y) == x

-- 6.
prop_caret :: Int -> Int -> Bool
prop_caret x y = x ^ y == y ^ x

-- 7.
prop_reverseList :: [a] -> Bool
prop_reverseList xs = reverse . reverse xs == xs
