module Ch14.Addition where

import Test.Hspec
import Test.QuickCheck

main :: IO ()
main =
  hspec $
  describe "Addition" $
  do it "1 + 1 is greater than 1" $ (1 + 1) > 1 `shouldBe` True
     it "2 + 2 is equal to 4" $ 2 + 2 `shouldBe` 4
     it "15 divided by 3 is 5" $ dividedBy 15 3 `shouldBe` (5, 0)
     it "22 divided by 5 is 4 remainder 2" $ dividedBy 22 5 `shouldBe` (4, 2)
     it "4 times 4 is equal to 24" $ recursiveMultiply 4 4 `shouldBe` 16
     it "0 times 10 is equal to 0" $ recursiveMultiply 0 10 `shouldBe` 0
     it "x + 1 is always greater than x" $ property $ \x -> x + 1 > (x :: Int)

prop_additionGreater
  :: (Num a, Ord a)
  => a -> Bool
prop_additionGreater x = x + 1 > x

dividedBy
  :: Integral a
  => a -> a -> (a, a)
dividedBy num denom = go num denom 0
  where
    go n d count
      | n < d = (count, n)
      | otherwise = go (n - d) d (count + 1)

recursiveMultiply
  :: (Integral a)
  => a -> a -> a
recursiveMultiply _ 0 = 0
recursiveMultiply n1 n2 = n1 + recursiveMultiply n1 (n2 - 1)

genBool :: Gen Bool
genBool = choose (False, True)

genBool' :: Gen Bool
genBool' = elements [True, False]

genOrdering :: Gen Ordering
genOrdering = elements [LT, EQ, GT]

genChar :: Gen Char
genChar = elements ['a' .. 'z']

genTuple
  :: (Arbitrary a, Arbitrary b)
  => Gen (a, b)
genTuple = do
  a <- arbitrary
  b <- arbitrary
  return (a, b)

genThreeple
  :: (Arbitrary a, Arbitrary b, Arbitrary c)
  => Gen (a, b, c)
genThreeple = do
  a <- arbitrary
  b <- arbitrary
  c <- arbitrary
  return (a, b, c)

genEither
  :: (Arbitrary a, Arbitrary b)
  => Gen (Either a b)
genEither = do
  a <- arbitrary
  b <- arbitrary
  elements [Left a, Right b]

genMaybe
  :: Arbitrary a
  => Gen (Maybe a)
genMaybe = do
  a <- arbitrary
  elements [Nothing, Just a]

genMaybe'
  :: Arbitrary a
  => Gen (Maybe a)
genMaybe' = do
  a <- arbitrary
  frequency [(1, return Nothing), (3, return (Just a))]

runQc = quickCheck prop_additionGreater
