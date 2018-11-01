module Ch15.OptionalMonoid where

import Data.Semigroup

data Optional a
  = Nada
  | Only a
  deriving (Eq, Show)

instance Semigroup a =>
         Semigroup (Optional a) where
  (<>) (Only x) (Only y) = Only $ x <> y
  (<>) (Only x) _ = Only x
  (<>) _ (Only y) = Only y
  (<>) _ _ = Nada

instance Monoid a =>
         Monoid (Optional a) where
  mempty = Nada
