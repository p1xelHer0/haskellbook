module Lookup where

import Data.List (elemIndex)
import Control.Applicative

f x = lookup x [(3, "hello"), (4, "julie"), (5, "kbai")]

g y = lookup y [(7, "sup?"), (8, "chris"), (9, "aloha")]

h z = lookup z [(2, 3), (5, 6), (7, 8)]

m x = lookup x [(4, 10), (8, 13), (1, 9001)]

-- 1.
added :: Maybe Integer
added = (+ 3) <$> (lookup 3 $ zip [1, 2, 3] [4, 5, 6])

-- 2.
y2 :: Maybe Integer
y2 = lookup 3 $ zip [1, 2, 3] [4, 5, 6]

z2 :: Maybe Integer
z2 = lookup 2 $ zip [1, 2, 3] [4, 5, 6]

tupled :: Maybe (Integer, Integer)
tupled = (,) <$> y2 <*> z2

-- 3.
x3 :: Maybe Int
x3 = elemIndex 3 [1, 2, 3, 4, 5]

y3 :: Maybe Int
y3 = elemIndex 4 [1, 2, 3, 4, 5]

max' :: Int -> Int -> Int
max' = max

maxed :: Maybe Int
maxed = max' <$> x3 <*> y3

-- 4.
xs4 = [1, 2, 3]

ys4 = [4, 5, 6]

x4 :: Maybe Integer
x4 = lookup 3 $ zip xs4 ys4

y4 :: Maybe Integer
y4 = lookup 2 $ zip xs4 ys4

summed :: Maybe Integer
summed = fmap sum $ (,) x4 <$> y4
