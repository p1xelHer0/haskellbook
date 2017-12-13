module ComprehendThyLists where

mySqr =
  [ x ^ 2
  | x <- [1 .. 10] ]

f1 =
  [ x
  | x <- mySqr
  , rem x 2 == 0 -- => [4, 16, 36, 64, 100]
    ]

f2 =
  [ (x, y)
  | x <- mySqr
  , y <- mySqr
  , x < 50
  , y > 50 ]

-- [(1, 64), (1, 81), (1, 100),
--  (4, 64), (4, 81), (4, 100),
--  (16, 64), (16, 81), (16, 100),
--  (36, 64), (36, 81), (36, 100),
f3 =
  take
    5
    [ (x, y)
    | x <- mySqr
    , y <- mySqr
    , x < 50
    , y > 50 ]

-- [(1, 64), (1, 81), (1, 100), (4, 64), (4, 18)]
-- }}}
-- Square Cube {{{
mySqr2 =
  [ x ^ 2
  | x <- [1 .. 5] ]

myCube2 =
  [ y ^ 3
  | y <- [1 .. 5] ]

tupleSqrCube =
  [ (x, y)
  | x <- mySqr2
  , y <- myCube2 ]

tupleSqrCube50 =
  [ (x, y)
  | x <- mySqr2
  , y <- myCube2
  , x < 50
  , y < 50 ]

tSC50Length = length tupleSqrCube50
