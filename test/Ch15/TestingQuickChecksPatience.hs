module Ch15.TestingQuickChecksPatience where

import Test.QuickCheck
import Data.Monoid

tests :: IO ()
tests = do
  putStrLn "Patience (supposed to fail!):"
  let ma = monoidAssoc
      mli = monoidLeftIdentity
      mlr = monoidRightIdentity
  quickCheck (ma :: BullMappend)
  quickCheck (mli :: Bull -> Bool)
  quickCheck (mlr :: Bull -> Bool)

monoidAssoc
  :: (Eq m, Monoid m)
  => m -> m -> m -> Bool
monoidAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity
  :: (Eq m, Monoid m)
  => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity
  :: (Eq m, Monoid m)
  => m -> Bool
monoidRightIdentity a = (a <> mempty) == a

data Bull
  = Fools
  | Twoo
  deriving (Eq, Show)

instance Arbitrary Bull where
  arbitrary = frequency [(1, return Fools), (1, return Twoo)]

instance Monoid Bull where
  mempty = Fools
  mappend _ _ = Fools

type BullMappend = Bull -> Bull -> Bull -> Bool
