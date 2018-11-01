module Ch15.MaybeAnotherMonoid where

import Test.QuickCheck

import Ch15.OptionalMonoid
import Ch15.TestingQuickChecksPatience

newtype First' a = First'
  { getFirst' :: Optional a
  } deriving (Eq, Show)

instance Semigroup (First' a) where
  (<>) (First' Nada) (First' y) =
    First'
    { getFirst' = y
    }
  (<>) (First' x) _ =
    First'
    { getFirst' = x
    }
  (<>) _ (First' y) =
    First'
    { getFirst' = y
    }
  (<>) _ _ =
    First'
    { getFirst' = Nada
    }

instance Monoid (First' a) where
  mempty =
    First'
    { getFirst' = Nada
    }

firstMappend :: First' a -> First' a -> First' a
firstMappend = mappend

type FirstMappend = First' String -> First' String -> First' String -> Bool

type FstId = First' String -> Bool

instance Arbitrary a =>
         Arbitrary (Optional a) where
  arbitrary = genOptional

genOptional
  :: Arbitrary a
  => Gen (Optional a)
genOptional = do
  a <- arbitrary
  oneof [return $ Only a, return Nada]

instance Arbitrary a =>
         Arbitrary (First' a) where
  arbitrary = genFirst

genFirst
  :: Arbitrary a
  => Gen (First' a)
genFirst = do
  a <- arbitrary
  return (First' a)

tests :: IO ()
tests = do
  putStrLn "MaybeAnotherMonoid:"
  quickCheck (monoidAssoc :: FirstMappend)
  quickCheck (monoidLeftIdentity :: FstId)
  quickCheck (monoidRightIdentity :: FstId)
