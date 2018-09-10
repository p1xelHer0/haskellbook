module ConstantInstance where

import Data.Monoid as M

newtype Constant a b = Constant
  { getConstant :: a
  } deriving (Eq, Ord, Show)

instance Functor (Constant a) where
  fmap f (Constant a) = Constant a

instance Monoid a =>
         Applicative (Constant a) where
  pure b = Constant mempty
  (Constant a1) <*> (Constant a2) = Constant (a1 M.<> a2)
