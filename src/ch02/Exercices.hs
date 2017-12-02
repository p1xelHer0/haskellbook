module CH2Exercises where

-- Parenthesization {{{
-- example.
-- 2 + 2 * 3 - 3
-- will produce
-- 2 + (2 * 3) - 3
-- do the same on the following:
-- 1.
-- 2 + 2 * 3 - 1
-- 2 + (2 * 3) - 1
-- 2.
-- (^) 10 $ 1 + 1
-- 10 ^ (1 + 1)
-- 3.
-- 2 ^ 2 * 4 ^ 5 + 1
-- (2 ^ 2) * (4 ^ 5) + 1
-- }}}
-- Equivalent expressions {{{
-- 1.
-- 1 + 1
-- 2
-- True
-- 2.
-- 10 ^ 2
-- 10 + 9 * 10
-- 10 + (9 * 10) -> 10 + 90 = 100
-- 10 ^ 2 = 10 * 10 = 100
-- True
-- 3.
-- 400 - 37
-- (-) 37 400
-- these argument are in the wrong order
-- this would result in 37 - 400
-- False
-- 4.
-- 100 `div` 3
-- 100 / 3
-- :info div
-- class (Real a, Enum a) => Integral a where
-- :info /
-- class Num a => Fractional a where
-- one is Integral, one is Fractional
-- False
-- 5.
-- 2 * 5 + 18
-- 2 * (5 + 18)
-- this is not the same since we explicitly
-- tell GHC to evaluate 5 + 18 before 2
-- False
-- }}}
-- More fun with functions {{{
-- example, these are defined
z = 7

x = y ^ 2

waxOn = x * 5

y = z + 8

-- evaluates to:
-- z = 7 -> constant
-- y = z + 8 -> 7 + 8 = 15, constant
-- x = y ^ 2 -> 15 ^ 2 = 225, constant
-- waxOn = x * 5 -> 225 * 5 = 1125, constant
-- what happens when we enter:
-- 1.
-- 10 + waxOn
-- 10 + 1125 = 1135
-- (+10) waxOn
-- (+10) will return a function which expects another argument
-- 10 + <missing argument>
-- the argument in our expression is waxOn
-- 10 + waxOn = 10 + 1125 = 1135
-- (-) 15 waxOn
-- 15 - waxOn = 15 - 1125 = -1110
-- (-) waxOn 15
-- waxOn - 15 = 1125 - 15 = 1110
-- 2.
-- what happens when we add the following?
triple x = x * 3

-- 3.
-- what will happen when we call it?
-- triple waxOn
-- we simply apply the function triple to the argument
-- waxOn is a constant so it becomes
-- triple waxOn = waxOn * 3
-- 1125 * 3 = 3375
-- 4 + 5.
-- this has been rewritte, as seen above
-- 6.
waxOff x = triple x-- this is the same thing as waxOff = triple
-- }}}
