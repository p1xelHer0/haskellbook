module MoreBottoms where

import Data.Bool

-- 1. bottom
-- 2. value, 2
-- 3. bottom
-- 4. returns a list that checks if the current letter is a vowel
-- 5.
-- a) [1, 4, 9, 16, 25, 36, 56, 64, 81, 100]
-- b) [1, 10, 20]
-- c) [15, 15, 15]
-- 6.
mapBool x = map (bool x) [1 .. 10]
