module Ch16.DetermineValidFunctors where

import GHC.Arr

-- 1.
-- no, kind is *, a functor needs * -> *

-- 2.
-- yes, this is isomorphic to Either

-- 3.
-- yes, this is Isomorphic to Maybe

-- 4.
newtype Mu f = InF { outF :: f (Mu f) }
-- :k Mu :: (* -> *) -> *, f needs to be a * -> *
-- i dont get this right now, gonna ask later

-- 5.
data D = D (Array Word Word) Int Int
-- no, kind is *, a functor needs * -> *
