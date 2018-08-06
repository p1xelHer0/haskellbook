module Ch15.Monoid where

import Data.Semigroup as S
import Data.Monoid as M
import Test.QuickCheck
import Test.Hspec

import Ch15.TestingQuickChecksPatience

tests :: IO ()
tests = do
  putStrLn "Monoid:"
  quickCheck (semigroupAssoc :: TrivAssoc)
  quickCheck (monoidLeftIdentity :: Trivial -> Bool)
  quickCheck (monoidRightIdentity :: Trivial -> Bool)
  quickCheck (semigroupAssoc :: IdentityAssoc)
  quickCheck (monoidLeftIdentity :: Identity String -> Bool)
  quickCheck (monoidRightIdentity :: Identity String -> Bool)
  quickCheck (semigroupAssoc :: TwoAssoc)
  quickCheck (monoidLeftIdentity :: Two String String -> Bool)
  quickCheck (monoidRightIdentity :: Two String String -> Bool)
  quickCheck (semigroupAssoc :: BoolConjAssoc)
  quickCheck (monoidLeftIdentity :: BoolConj -> Bool)
  quickCheck (monoidRightIdentity :: BoolConj -> Bool)
  quickCheck (semigroupAssoc :: BoolDisjAssoc)
  quickCheck (monoidLeftIdentity :: BoolDisj -> Bool)
  quickCheck (monoidRightIdentity :: BoolDisj -> Bool)

semigroupAssoc
  :: (Eq m, Semigroup m)
  => m -> m -> m -> Bool
semigroupAssoc a b c = (a S.<> (b S.<> c)) == ((a S.<> b) S.<> c)

-- 1.
data Trivial =
  Trivial
  deriving (Eq, Show)

instance Semigroup Trivial where
  _ <> _ = Trivial

instance Monoid Trivial where
  mempty = Trivial
  mappend = (S.<>)

instance Arbitrary Trivial where
  arbitrary = return Trivial

type TrivAssoc = Trivial -> Trivial -> Trivial -> Bool

-- 2.
newtype Identity a =
  Identity a
  deriving (Eq, Show)

instance Semigroup a =>
         Semigroup (Identity a) where
  (Identity x) <> (Identity y) = Identity $ x S.<> y

instance Semigroup a => Monoid (Identity a) where
  mempty = undefined
  mappend = (S.<>)

instance Arbitrary a =>
         Arbitrary (Identity a) where
  arbitrary = genIdentity

genIdentity
  :: Arbitrary a
  => Gen (Identity a)
genIdentity = do
  a <- arbitrary
  return (Identity a)

type IdentityAssoc = Identity String -> Identity String -> Identity String -> Bool

-- 3.
data Two a b =
  Two a
      b
  deriving (Eq, Show)

instance (Semigroup a, Semigroup b) =>
         Semigroup (Two a b) where
  (Two x1 y1) <> (Two x2 y2) = Two (x1 S.<> x2) (y1 S.<> y2)

instance (Semigroup a, Monoid a, Semigroup b, Monoid b) =>
         Monoid (Two a b) where
  mempty = Two mempty mempty
  mappend = (S.<>)

instance (Arbitrary a, Arbitrary b) =>
         Arbitrary (Two a b) where
  arbitrary = genTwo

genTwo
  :: (Arbitrary a, Arbitrary b)
  => Gen (Two a b)
genTwo = do
  a <- arbitrary
  b <- arbitrary
  return (Two a b)

type TwoAssoc = Two String String -> Two String String -> Two String String -> Bool

-- 4.
newtype BoolConj =
  BoolConj Bool
  deriving (Eq, Show)

instance Semigroup BoolConj where
  (BoolConj True) <> (BoolConj True) = BoolConj True
  _ <> _ = BoolConj False

instance Monoid BoolConj where
  mempty = BoolConj True
  mappend = (S.<>)

instance Arbitrary BoolConj where
  arbitrary = genBoolConj

genBoolConj :: Gen BoolConj
genBoolConj = do
  a <- arbitrary
  return (BoolConj a)

type BoolConjAssoc = BoolConj -> BoolConj -> BoolConj -> Bool

-- 5.
newtype BoolDisj =
  BoolDisj Bool
  deriving (Eq, Show)

instance Semigroup BoolDisj where
  (BoolDisj True) <> _ = BoolDisj True
  _ <> (BoolDisj True) = BoolDisj True
  _ <> _ = BoolDisj False

instance Monoid BoolDisj where
  mempty = BoolDisj False
  mappend = (S.<>)

instance Arbitrary BoolDisj where
  arbitrary = genBoolDisj

genBoolDisj :: Gen BoolDisj
genBoolDisj = do
  a <- arbitrary
  return (BoolDisj a)

type BoolDisjAssoc = BoolDisj -> BoolDisj -> BoolDisj -> Bool

-- 6.
newtype Combine a b = Combine
  { unCombine :: a -> b
  }

f = Combine $ \n -> Sum (n + 1)

g = Combine $ \n -> Sum (n - 1)
