module StandardFunctions where

import Data.Char

-- here's how you might write your own version of it:
-- direct recursion, not using (&&)
myAnd :: [Bool] -> Bool
myAnd [] = True
myAnd (x:xs) =
  if x == False
    then False
    else myAnd xs

-- direct recursion, using (&&)
myAnd2 :: [Bool] -> Bool
myAnd2 [] = True
myAnd2 (x:xs) = x && myAnd xs

-- 1. myOr returns True if any Bool in the list is True
myOr :: [Bool] -> Bool
myOr [] = False
myOr (x:xs) = x || myOr xs

-- 2. myAy returns True if a -> Bool applied
-- to any of the values in the list returns True
myAny :: (a -> Bool) -> [a] -> Bool
myAny _ [] = False
myAny f (x:xs) = f x || myAny f xs

-- 3.1. myAny returns True if the element exists in the list
myElem
  :: Eq a
  => a -> [a] -> Bool
myElem _ [] = False
myElem x (y:ys) = x == y || myElem x ys

-- 3.2. write another version that uses any
myElemWithAny
  :: Eq a
  => a -> [a] -> Bool
myElemWithAny _ [] = False
myElemWithAny x y = any (== x) y

-- 4. myReverse [1..5] == [5,4,3,2,1]
myReverse :: [a] -> [a]
myReverse [] = []
myReverse xs = last xs : myReverse (init xs)

-- 5. squish flattens a list of lists into a lists
squish :: [[a]] -> [a]
squish [] = []
squish (x:xs) = x ++ squish xs

-- 6. squishMap maps a function over a list and concatenates the result
squishMap :: (a -> [b]) -> [a] -> [b]
squishMap _ [] = []
squishMap f (x:xs) = f x ++ squishMap f xs

-- 7.squishAgain fattens a list of lists into a list. This time re-use the squishMap function
squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

-- 8. myMaximumBy takes a comparison function and a list and returns the
--    greatest element of the list based on the last value that the comparison
--    returned GT for
-- let xs = [1, 53, 9001, 10]
-- myMaximum compare xs
-- 9001
myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy _ [x] = x
myMaximumBy f (x:xs) =
  case f x y of
    LT -> y
    EQ -> x
    GT -> x
  where y = myMaximumBy f xs

-- 9. myMinimumBy takes a comparison function and a list and returns the
--    least element of the list based on the last value that the comparison
--    returned LT for
-- let xs = [1, 53, 9001, 10]
-- myMaximum compare xs
-- 1
myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy _ [x] = x
myMinimumBy f (x:xs) =
  case f x y of
    LT -> x
    EQ -> x
    GT -> y
  where y = myMinimumBy f xs
