module Ch17.ValidateTheInstances where

import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

-- 1.
data Pair a =
  Pair a
       a
  deriving (Eq, Show)

instance Functor Pair where
  fmap f (Pair x y) = Pair (f x) (f y)

instance Applicative Pair where
  pure x = Pair x x
  Pair f1 f2 <*> Pair x y = Pair (f1 x) (f2 y)

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

instance Eq a =>
         EqProp (Pair a) where
  (=-=) = eq

-- 2.
data Two a b =
  Two a
      b
  deriving (Eq, Show)

instance Functor (Two a) where
  fmap f (Two x y) = Two x (f y)

instance Monoid a =>
         Applicative (Two a) where
  pure = Two mempty
  Two x f1 <*> Two y f2 = Two (x <> y) (f1 f2)

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

instance (Eq a, Eq b) =>
         EqProp (Two a b) where
  (=-=) = eq

-- 3.
data Three a b c =
  Three a
        b
        c
  deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three x y z) = Three x y (f z)

instance (Monoid a, Monoid b) =>
         Applicative (Three a b) where
  pure = Three mempty mempty
  Three f1 f2 f3 <*> Three x y z = Three (f1 <> x) (f2 <> y) (f3 z)

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

instance (Eq a, Eq b, Eq c) =>
         EqProp (Three a b c) where
  (=-=) = eq

-- 4.
data Three' a b =
  Three' a
         b
         b
  deriving (Eq, Show)

instance Functor (Three' a) where
  fmap f (Three' x y z) = Three' x (f y) (f z)

instance Monoid a =>
         Applicative (Three' a) where
  pure x = Three' mempty x x
  Three' f1 f2 f3 <*> Three' x y z = Three' (f1 <> x) (f2 y) (f3 z)

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

instance (Eq a, Eq b) =>
         EqProp (Three' a b) where
  (=-=) = eq

-- 5.
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

instance (Monoid a, Monoid b, Monoid c) =>
         Applicative (Four a b c) where
  pure = Four mempty mempty mempty
  Four f1 f2 f3 f4 <*> Four x y z a = Four (f1 <> x) (f2 <> y) (f3 <> z) (f4 a)

genFour
  :: (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d)
  => Gen (Four a b c d)
genFour = do
  a <- arbitrary
  b <- arbitrary
  c <- arbitrary
  d <- arbitrary
  return (Four a b c d)

instance (Eq a, Eq b, Eq c, Eq d) =>
         EqProp (Four a b c d) where
  (=-=) = eq

-- 6.
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

instance Monoid a =>
         Applicative (Four' a) where
  pure = Four' mempty mempty mempty
  Four' f1 f2 f3 f4 <*> Four' x y z a =
    Four' (f1 <> x) (f2 <> y) (f3 <> z) (f4 a)

genFour'
  :: (Arbitrary a, Arbitrary b)
  => Gen (Four' a b)
genFour' = do
  a <- arbitrary
  b <- arbitrary
  c <- arbitrary
  d <- arbitrary
  return (Four' a b c d)

instance (Eq a, Eq b) =>
         EqProp (Four' a b) where
  (=-=) = eq

tests :: IO ()
tests = do
  putStrLn "ValidateTheInstances: "
  putStrLn "Pair: "
  quickBatch $
    applicative (undefined :: Pair (String, Sum Integer, Product Integer))
  putStrLn "Two: "
  quickBatch $
    applicative
      (undefined :: Two (String, Sum Integer, Product Integer) (String, Sum Integer, Product Integer))
  putStrLn "Three: "
  quickBatch $
    applicative
      (undefined :: Three (String, Sum Integer, Product Integer) (String, Sum Integer, Product Integer) (String, Sum Integer, Product Integer))
  putStrLn "Three': "
  quickBatch $
    applicative
      (undefined :: Three' (String, Sum Integer, Product Integer) (String, Sum Integer, Product Integer))
  putStrLn "Four: "
  quickBatch $
    applicative
      (undefined :: Four (String, Sum Integer, Product Integer) (String, Sum Integer, Product Integer) (String, Sum Integer, Product Integer) (String, Sum Integer, Product Integer))
  putStrLn "Four': "
  quickBatch $
    applicative
      (undefined :: Four' (String, Sum Integer, Product Integer) (String, Sum Integer, Product Integer))
