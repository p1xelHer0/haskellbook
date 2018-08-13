module Ch16.Possibly where

import Test.QuickCheck

import Ch16.FunctorProp

tests :: IO ()
tests = do
  putStrLn "Possibly:"
  quickCheck possiblyFID
  quickCheck (functorCompose' :: PossiblyFC)

-- 1.
data Possibly a
  = LolNope
  | Yeppers a
  deriving (Eq, Show)

instance Functor Possibly where
  fmap f (Yeppers a) = Yeppers (f a)
  fmap _ LolNope = LolNope

instance Arbitrary a =>
         Arbitrary (Possibly a) where
  arbitrary = genPossibly

genPossibly
  :: Arbitrary a
  => Gen (Possibly a)
genPossibly = do
  a <- arbitrary
  frequency [(1, return LolNope), (1, return (Yeppers a))]

possiblyFID :: Possibly Int -> Bool
possiblyFID = functorIdentity

type PossiblyFC = Possibly Int -> IntToInt -> IntToInt -> Bool
