module Ch17.BadMonoid where

import Data.Monoid
import Test.Quickcheck
import Test.Quickcheck.Checkers
import Test.QuickCheck.Classes

data Bull
  = Fools
  | Twoo
  deriving (Eq, Show)

instance Arbitrary Bull where
  arbitrary = frequency [(1, return Fools), (1, return Twoo)]

instance Monoid Bull where
  mempty = Fools
  mappend _ _ = Fools

instance EqProp Bull where
  (=-=) = eq

tests :: IO ()
tests = do
  putStrLn "BadMonoid:"
  quickBatch (monoid Twoo)
