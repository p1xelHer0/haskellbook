module Ch17.VariationsOnEither where

import Data.Semigroup
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Validate e a
  = Fail e
  | Succ a
  deriving (Eq, Show)

instance Functor (Validate e) where
  fmap f (Fail e) = Fail e
  fmap f (Succ a) = Succ (f a)

instance Monoid e =>
         Applicative (Validate e) where
  pure = Succ
  Fail e1 <*> Fail e2 = Fail (e1 <> e2)
  _ <*> Fail e2 = Fail e2
  Fail e1 <*> _ = Fail e1
  Succ f <*> Succ a = Succ (f a)

instance (Arbitrary e, Arbitrary a) =>
         Arbitrary (Validate e a) where
  arbitrary = genList

genList
  :: (Arbitrary e, Arbitrary a)
  => Gen (Validate e a)
genList = do
  e <- arbitrary
  a <- arbitrary
  frequency [(1, return (Fail e)), (5, return (Succ a))]

instance (Eq e, Eq a) =>
         EqProp (Validate e a) where
  (=-=) = eq

tests :: IO ()
tests = do
  putStrLn "VariationsOnEither: "
  quickBatch $
    applicative
      (undefined :: Validate (String, Sum Integer, Product Integer) (String, Product Integer, Sum Integer))
