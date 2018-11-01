module Ch17.ListApplicative where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data List a
  = Nil
  | Cons a
         (List a)
  deriving (Eq, Show)

append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys = Cons x $ xs `append` ys

fold :: (a -> b -> b) -> b -> List a -> b
fold _ b Nil = b
fold f b (Cons h t) = f h (fold f b t)

concat' :: List (List a) -> List a
concat' = fold append Nil

flatMap :: (a -> List b) -> List a -> List b
flatMap f xs = concat' $ fmap f xs

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Applicative List where
  pure x = Cons x Nil
  Nil <*> _ = Nil
  _ <*> Nil = Nil
  Cons f fs <*> xs = append (fmap f xs) (fs <*> xs)

instance Arbitrary a =>
         Arbitrary (List a) where
  arbitrary = genList

genList
  :: Arbitrary a
  => Gen (List a)
genList = do
  a <- arbitrary
  frequency [(5, return (Cons a Nil)), (1, return Nil)]

instance Eq a =>
         EqProp (List a) where
  (=-=) = eq

tests :: IO ()
tests = do
  putStrLn "ListApplicative: "
  quickBatch $ applicative (undefined :: List (String, Char, Integer))
