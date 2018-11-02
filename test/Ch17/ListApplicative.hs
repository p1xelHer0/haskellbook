module Ch17.ListApplicative where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

-- List
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

-- ZipList
newtype ZipList' a =
  ZipList' (List a)
  deriving (Eq, Show)

take' :: Int -> List a -> List a
take' 0 _ = Nil
take' _ Nil = Nil
take' n (Cons x xs) = Cons x (take' (n - 1) xs)

instance Functor ZipList' where
  fmap f (ZipList' xs) = ZipList' $ fmap f xs

instance Applicative ZipList' where
  pure x = ZipList' (Cons x Nil)
  ZipList' Nil <*> _ = ZipList' Nil
  _ <*> ZipList' Nil = ZipList' Nil
  ZipList' (Cons f fs) <*> ZipList' (Cons x xs) =
    ZipList' (Cons (f x) (fs <*> xs))

instance Arbitrary a =>
         Arbitrary (ZipList' a) where
  arbitrary = genZipList

genZipList
  :: Arbitrary a
  => Gen (ZipList' a)
genZipList = do
  a <- arbitrary
  return (ZipList' a)

instance Eq a =>
         EqProp (ZipList' a) where
  xs =-= ys = xs' `eq` ys'
    where
      xs' =
        let (ZipList' l) = xs
        in take' 3000 l
      ys' =
        let (ZipList' l) = ys
        in take' 3000 l

tests :: IO ()
tests = do
  putStrLn "ListApplicative: "
  quickBatch $ applicative (undefined :: List (String, Char, Integer))
  putStrLn "ZipListApplicative: "
  quickBatch $ applicative (undefined :: ZipList' (String, Char, Integer))
