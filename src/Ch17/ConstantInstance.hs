module Ch17.ConstantInstance where

import Data.Monoid as M

newtype Constant a b = Constant
  { getConstant :: a
  } deriving (Eq, Ord, Show)

instance Functor (Constant a) where
  fmap f (Constant a) = Constant a

instance Monoid a =>
         Applicative (Constant a) where
  pure b = Constant mempty
  (Constant a) <*> (Constant b) = Constant (a M.<> b)
