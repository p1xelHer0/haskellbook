module Ch14.GenRandomGenerator where

import Test.QuickCheck

tests :: IO ()
tests = do
  putStrLn "GenRandomGenerator: "

data Fool1
  = Fulse1
  | Frue1
  deriving (Eq, Show)

data Fool2
  = Fulse2
  | Frue2
  deriving (Eq, Show)

-- 1.
instance Arbitrary Fool1 where
  arbitrary = elements [Fulse1, Frue1]

-- 2.
instance Arbitrary Fool2 where
  arbitrary = frequency [(2, return Fulse2), (6, return Frue2)]
