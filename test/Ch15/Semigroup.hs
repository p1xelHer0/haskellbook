module Ch15.Semigroup where

import Data.Semigroup as S

import Test.QuickCheck

tests :: IO ()
tests = do
  putStrLn "Semigroup:"
  quickCheck (semigroupAssoc :: TrivAssoc)

data Trivial =
  Trivial
  deriving (Eq, Show)

instance Semigroup Trivial where
  _ <> _ = undefined

instance Arbitrary Trivial where
  arbitrary = return Trivial

semigroupAssoc
  :: (Eq m, Semigroup m)
  => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

type TrivAssoc = Trivial -> Trivial -> Trivial -> Bool
