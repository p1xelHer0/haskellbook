{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

module LogicGoats where

class TooMany a where
  tooMany :: a -> Bool

instance TooMany Int where
  tooMany n = n > 42

newtype Goats =
  Goats Int
  deriving (Eq, Show, TooMany)

-- 1.
instance TooMany (Int, String) where
  tooMany (n, _) = n > 42

-- 2.
instance TooMany (Int, Int) where
  tooMany (n, m) = n + m > 42

-- 3. TODO
instance TooMany (Num a, TooMany a) => (a, a) where
  tooMany (n, m) = tooMany (n, m)
