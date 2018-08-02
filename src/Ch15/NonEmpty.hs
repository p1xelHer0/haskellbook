module NonEmpty where

import Data.List.NonEmpty as N
import Data.Semigroup as S

a = 1 :| [2, 3]

xs = 1 :| [2, 3]

ys = 4 :| [5, 6]

xsyx = xs <> ys

h = N.head xs

l = N.length (xs <> ys)
