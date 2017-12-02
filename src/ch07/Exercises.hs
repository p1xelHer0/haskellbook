module CH7Exercises where

-- Grab Bag {{{
-- 1.
-- a)
mTha x y z = x * y * z

-- b)
mThb x y = \z -> x * y * z

-- c)
mThc x = \y -> \z -> x * y * z

-- d)
mThd = \x -> \y -> \z -> x * y * z

-- they are all the same!
-- 2.
-- the typeclass is a) Integer -> Integer -> Integer
-- no, it's d) Num a => a -> a -> a!
-- 3.
-- a)
addOneIfOdd n =
  case odd n of
    True -> f n
    False -> n
  where
    f = \n -> n + 1

-- b)
addFive =
  \x ->
     \y ->
        (if x > y
           then y
           else x) +
        5

-- c)
mflip f x y = f y x -- }}}

-- Variety Pack {{{
-- 1.
k (x, y) = x

k1 = k ((4 - 1), 10)

k2 = k ("three", (1 + 2))

k3 = k (3, True)

-- a)
-- a -> b -> a
-- b)
-- k2 :: String, no, k1 and k3 are both Integer
-- b)
-- k1 and k2 will
--
-- 2.
f22 :: (a, b, c) -> (d, e, f) -> ((a, d), (c, f))
f22 (a, _, c) (d, _, f) = ((a, d), (c, f))

-- }}}
--
-- Case Practice {{{
-- 1.
functionC x y =
  case x > y of
    True -> x
    False -> y

-- 2.
ifEvenAdd2 n =
  case even n of
    True -> n + 2
    False -> n

-- 3.
nums x =
  case compare x 0 of
    EQ -> 0
    LT -> -1
    GT -> 1

-- }}}
--
-- Artful Dodgy {{{
dodgy x y = x + y * 10

oneIsOne = dodgy 1

onIsTwo = (flip dodgy) 2

-- 1.
-- 1
-- 2.
-- 11
-- 3.
-- 22
-- 4.
-- 21
-- 5.
-- 12
-- 6.
-- 11
-- 7.
-- 21
-- 8.
-- 21
-- 9.
-- 22
-- 10.
-- 31
-- 11.
-- 23
-- }}}
--
-- Guard Duty {{{
-- 1.
avgGrade1
  :: (Fractional a, Ord a)
  => a -> Char
avgGrade1 x
  | y >= 0.9 = 'A'
  | y >= 0.8 = 'B'
  | y >= 0.7 = 'C'
  | y >= 0.59 = 'D'
  -- | y < 0.59 = 'F'
  -- to me, this makes more sense, and works the same?
  | otherwise = 'F'
  where
    y = x / 100

-- 2.
avgGrade2
  :: (Fractional a, Ord a)
  => a -> Char
avgGrade2 x
  | y >= 0.7 = 'C'
  | y >= 0.9 = 'A'
  | y >= 0.8 = 'B'
  | y >= 0.59 = 'D'
  | otherwise = 'F'
  where
    y = x / 100

-- since the first guard matches, it will return 'C'!
--
-- 3.
pal xs
  | xs == reverse xs = True
  | otherwise = False

-- b) - True when xs is a palindrome
--
-- 4.
-- [a] a List of anything
--
-- 5
-- reverse :: [a] -> [a], a list of anything
--
-- 6.
numbers x
  | x < 0 = -1
  | x == 0 = 0
  | x > 0 = 1 -- c) - an indication of whether its argument is a positive or negative number or zero

--
-- 7.
-- Ord a
--
-- 8.
-- numbers :: (Ord a, Num a, Num t) => a -> t
-- }}}
--
-- Multiple choice {{{
-- 1.
-- d)
--
-- 2.
-- b)
--
-- 3.
-- d)
--
-- 4.
-- b)
--
-- 5.
-- a)
--
-- }}}
-- Let's write code {{{
-- 1.
tensDigit
  :: Integral a
  => a -> a
tensDigit x = d
  where
    xLast = x `div` 10
    d = xLast `mod` 10

-- a)
tensD
  :: Integral a
  => a -> a
tensD x = d
  where
    (xLast, _) = x `divMod` 10
    (_, d) = xLast `divMod` 10

-- b)
-- yes, it's the same type
-- 3)
hunsD x = d
  where
    (xLast, _) = x `divMod` 100
    (_, d) = xLast `divMod` 10

-- or like this
-- I don't fully understand this o_o
-- Wait I do we are just removing the last digit in the value and passing it
-- to the previous function!
hunsD2 = tensD . (`div` 10)

-- }}}
--
-- 2. {{{
foldBool3 :: a -> a -> Bool -> a
foldBool3 x _ False = x
foldBool3 _ y True = y

foldBool1 :: a -> a -> Bool -> a
foldBool1 x y z =
  case z of
    True -> y
    False -> x

foldBool2 :: a -> a -> Bool -> a
foldBool2 x y z
  | z = y
  | otherwise = x

-- }}}
--
-- 3. {{{
g :: (a -> b) -> (a, c) -> (b, c)
g f (a, c) = (f a, c)

-- }}}
--
-- 4. {{{
roundTrip
  :: (Show a, Read a)
  => a -> a
roundTrip a = read (show a)

-- print (roundTrip 4)
-- print (id 4)
-- }}}
-- 5. {{{
roundTripPF
  :: (Show a, Read a)
  => a -> a
roundTripPF = read . show

-- }}}
-- 6. {{{
roundTripAB
  :: (Show a, Read b)
  => a -> b
roundTripAB = read . show
-- (roundTripAB 4 :: Int)
-- }}}
