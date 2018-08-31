module Ch16.ShortExercise where

import Test.QuickCheck

import Ch16.FunctorProp

tests :: IO ()
tests = do
  putStrLn "Either:"
  quickCheck sumFID
  quickCheck (functorCompose' :: SumFC)

-- 1.
data Sum a b
  = First a
  | Second b
  deriving (Eq, Show)

instance Functor (Sum a) where
  fmap _ (First x) = First x
  fmap f (Second y) = Second (f y)

instance (Arbitrary a, Arbitrary b) =>
         Arbitrary (Sum a b) where
  arbitrary = genSum

genSum
  :: (Arbitrary a, Arbitrary b)
  => Gen (Sum a b)
genSum = do
  a <- arbitrary
  b <- arbitrary
  frequency [(1, return (First a)), (1, return (Second b))]

sumFID :: Sum Int Int -> Bool
sumFID = functorIdentity

type SumFC = Sum Int Int -> IntToInt -> IntToInt -> Bool
