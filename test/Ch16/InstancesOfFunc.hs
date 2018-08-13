module Ch16.InstancesOfFunc where

import Test.QuickCheck

import Ch16.FunctorProp

tests :: IO ()
tests = do
  putStrLn "InstanceOfFunc:"
  quickCheck fc'
  quickCheck identityFID
  quickCheck (functorCompose' :: IdentityFC)
  quickCheck pairFID
  quickCheck (functorCompose' :: PairFC)
  quickCheck twoFID
  quickCheck (functorCompose' :: TwoFC)
  quickCheck threeFID
  quickCheck (functorCompose' :: ThreeFC)
  quickCheck threeFID'
  quickCheck (functorCompose' :: ThreeFC')
  quickCheck fourFID
  quickCheck (functorCompose' :: FourFC)
  quickCheck fourFID'
  quickCheck (functorCompose' :: FourFC')

-- 1.
newtype Identity a =
  Identity a
  deriving (Eq, Show)

instance Functor Identity where
  fmap f (Identity x) = Identity (f x)

instance Arbitrary a =>
         Arbitrary (Identity a) where
  arbitrary = genIdentity

genIdentity
  :: Arbitrary a
  => Gen (Identity a)
genIdentity = do
  a <- arbitrary
  return (Identity a)

identityFID :: Identity Int -> Bool
identityFID = functorIdentity

type IdentityFC = Identity Int -> IntToInt -> IntToInt -> Bool

-- 2.
data Pair a =
  Pair a
       a
  deriving (Eq, Show)

instance Functor Pair where
  fmap f (Pair x y) = Pair (f x) (f y)

instance Arbitrary a =>
         Arbitrary (Pair a) where
  arbitrary = genPair

genPair
  :: Arbitrary a
  => Gen (Pair a)
genPair = do
  a <- arbitrary
  b <- arbitrary
  return (Pair a b)

pairFID :: Pair Int -> Bool
pairFID = functorIdentity

type PairFC = Pair Int -> IntToInt -> IntToInt -> Bool

-- 3.
data Two a b =
  Two a
      b
  deriving (Eq, Show)

instance Functor (Two a) where
  fmap f (Two x y) = Two x (f y)

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

twoFID :: Two Int Int -> Bool
twoFID = functorIdentity

type TwoFC = Two Int Int -> IntToInt -> IntToInt -> Bool

-- 4.
data Three a b c =
  Three a
        b
        c
  deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three x y z) = Three x y (f z)

instance (Arbitrary a, Arbitrary b, Arbitrary c) =>
         Arbitrary (Three a b c) where
  arbitrary = genThree

genThree
  :: (Arbitrary a, Arbitrary b, Arbitrary c)
  => Gen (Three a b c)
genThree = do
  a <- arbitrary
  b <- arbitrary
  c <- arbitrary
  return (Three a b c)

threeFID :: Three Int Int Int -> Bool
threeFID = functorIdentity

type ThreeFC = Three Int Int Int -> IntToInt -> IntToInt -> Bool

-- 5.
data Three' a b =
  Three' a
         b
         b
  deriving (Eq, Show)

instance Functor (Three' a) where
  fmap f (Three' x y z) = Three' x (f y) (f z)

instance (Arbitrary a, Arbitrary b) =>
         Arbitrary (Three' a b) where
  arbitrary = genThree'

genThree'
  :: (Arbitrary a, Arbitrary b)
  => Gen (Three' a b)
genThree' = do
  a <- arbitrary
  b <- arbitrary
  c <- arbitrary
  return (Three' a b c)

threeFID' :: Three' Int Int -> Bool
threeFID' = functorIdentity

type ThreeFC' = Three' Int Int -> IntToInt -> IntToInt -> Bool

-- 6.
data Four a b c d =
  Four a
       b
       c
       d
  deriving (Eq, Show)

instance Functor (Four a b c) where
  fmap f (Four x y z i) = Four x y z (f i)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) =>
         Arbitrary (Four a b c d) where
  arbitrary = genFour

genFour
  :: (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d)
  => Gen (Four a b c d)
genFour = do
  a <- arbitrary
  b <- arbitrary
  c <- arbitrary
  d <- arbitrary
  return (Four a b c d)

fourFID :: Four Int Int Int Int -> Bool
fourFID = functorIdentity

type FourFC = Four Int Int Int Int -> IntToInt -> IntToInt -> Bool

-- 7.
data Four' a b =
  Four' a
        a
        a
        b
  deriving (Eq, Show)

instance Functor (Four' a) where
  fmap f (Four' x y z i) = Four' x y z (f i)

instance (Arbitrary a, Arbitrary b) =>
         Arbitrary (Four' a b) where
  arbitrary = genFour'

genFour'
  :: (Arbitrary a, Arbitrary b)
  => Gen (Four' a b)
genFour' = do
  a <- arbitrary
  b <- arbitrary
  c <- arbitrary
  d <- arbitrary
  return (Four' a b c d)

fourFID' :: Four' Int Int -> Bool
fourFID' = functorIdentity

type FourFC' = Four' Int Int -> IntToInt -> IntToInt -> Bool

-- 8.
data Trivial =
  Trivial -- we can't implement this since the kind of the type is *
-- we need a * -> *
