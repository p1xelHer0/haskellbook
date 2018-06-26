{-# LANGUAGE ScopedTypeVariables #-}

module Ch14.UsingQuickCheck where

import Data.List (sort)
import Test.QuickCheck

tests :: IO ()
tests = do
  putStrLn "UsingQuickCheck: "
  quickCheck prop_halfLessThanWhole
  quickCheck prop_sortedList
  quickCheck prop_plusAssociative
  quickCheck prop_plusCommutative
  quickCheck prop_multiplucationAssociative
  quickCheck prop_multiplucationCommutative
  quickCheck prop_quotRem
  quickCheck prop_quotRem2
  quickCheck prop_divMod
  quickCheck prop_reverseList
  quickCheck $ \(Blind (f :: Int -> Char)) a -> prop_dollarThingOne f a
  quickCheck prop_dollarThingOne2
  quickCheck $
    \(Blind (f :: Int -> Char)) (Blind (g :: Char -> Int)) a ->
       prop_dollarThingTwo f g a
  quickCheck prop_dollarThingTwo2
  quickCheck f1
  quickCheck f2
  quickCheck squareIdentity

-- quickCheck prop_dollarThingTwo
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
    go y (Just x, _) = (Just y, x >= y)

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
prop_quotRem :: Integer -> NonZero Integer -> Bool
prop_quotRem x (NonZero y) = quot x y * y + rem x y == x

prop_divMod :: Int -> NonZero Int -> Bool
prop_divMod x (NonZero y) = div x y * y + mod x y == x

-- alternative, using Gen to create positive Ints only
genPositive :: Gen Int
genPositive = abs `fmap` (arbitrary :: Gen Int) `suchThat` (> 0)

prop_quotRem2 :: Property
prop_quotRem2 =
  forAll genPositive $ \x -> forAll genPositive $ \y -> prop_quotRem2' x y

prop_quotRem2' :: Int -> Int -> Bool
prop_quotRem2' x y = quot x y * y + rem x y == x

prop_divMod2 :: Property
prop_divMod2 =
  forAll genPositive $ \x -> forAll genPositive $ \y -> prop_quotRem2' x y

prop_divMod2' :: Int -> Int -> Bool
prop_divMod2' x y = div x y * y + mod x y == x

-- 6. this is not true
-- prop_caret :: Int -> Int -> Bool
-- prop_caret x y = x ^ y == y ^ x
-- 7.
-- genInt = arbitrary :: Gen Int
-- prop_reverseList :: Property
-- prop_reverseList = forAll genInt prop_reverseList'
prop_reverseList :: [Int] -> Bool
prop_reverseList xs = (reverse . reverse) xs == xs

-- 8.
prop_dollarThingOne
  :: (Eq b)
  => (a -> b) -> a -> Bool
prop_dollarThingOne f a = (f $ a) == f a

prop_dollarThingOne2 :: Blind (Int -> Char) -> Int -> Bool
prop_dollarThingOne2 (Blind f) a = (f $ a) == f a

prop_dollarThingTwo
  :: (Eq b)
  => (a -> b) -> (b -> a) -> b -> Bool
prop_dollarThingTwo f g a = (f . g) a == (\x -> f (g x)) a

prop_dollarThingTwo2 :: Blind (Int -> Char) -> Blind (Char -> Int) -> Char -> Bool
prop_dollarThingTwo2 (Blind f) (Blind g) a = (f . g) a == (\x -> f (g x)) a

-- 9. TODO
-- prop_foldr :: [a] -> Bool
-- prop_foldr xs = (foldr (:) == (++)) xs == (foldr ((++) []) == concat) xs
-- 10. Not true for []
f1 :: Int -> [Int] -> Bool
f1 n xs = length (take n xs) == n

-- 11.
f2 :: Int -> Bool
f2 x = (read (show x)) == x

-- Failure
square x = x * x

squareIdentity :: Float -> Bool
squareIdentity x = (square . sqrt) x == x
