module ItsOnlyNatural where

data Nat
  = Zero
  | Succ Nat
  deriving (Eq, Show)

-- >>> natToInteger Zero
-- 0
-- >>> natToInteger (Succ Zero)
-- 1
-- >>> natToInteger (Succ (Succ Zero))
-- 2
natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ a) = 1 + natToInteger a

-- >>> integerToNat 0
-- Just Zero
-- >>> integerToNat 1
-- Just (Succ Zero)
-- >>> integerToNat 2
-- Just (Succ (Succ Zero))
-- >>> integerToNat (-1)
-- Nothing
-- integerToNat :: Integer -> Maybe Nat
integerToNat 0 = Just Zero
integerToNat a = Just (go a) where
  go 0 = Zero
  go a = Succ (go (a - 1))
