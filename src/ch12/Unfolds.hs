module Unfolds where

-- because it never ends, we must use take to get a finite list
-- take 10 $ iterate (+1) 0
-- [0,1,2,3,4,5,6,7,8,9]
myIterate :: (a -> a) -> a -> [a]
myIterate f x = go f [x]
  where
    go f (x:xs) = x : go f (f x : xs)

-- Using unfoldr to do
-- the same thing as iterate
-- take 10 $ unfoldr (\b -> Just (b, b+1)) 0
-- [0,1,2,3,4,5,6,7,8,9]
myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f x =
  case f x of
    Nothing -> []
    Just (a, b) -> a : myUnfoldr f b

-- It helps to have the
-- types in front of you
-- myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
betterIterate :: (a -> a) -> a -> [a]
betterIterate f = myUnfoldr (\b -> Just (b, f b))
