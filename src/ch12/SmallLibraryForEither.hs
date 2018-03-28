module SmallLibraryForEither where

lefts' :: [Either a b] -> [a]
lefts' (Right x:xs) = lefts' xs
lefts' (Left x:xs) = x : lefts' xs
lefts' _ = []

rights' :: [Either a b] -> [b]
rights' (Right x:xs) = x : rights' xs
rights' (Left x:xs) = rights' xs
rights' _ = []

-- something something
lefts'' :: [Either a b] -> [a]
lefts'' = undefined

partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' xs = (lefts' xs, rights' xs)

eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' f e = go $ f e where
  go (Left _) = Nothing
  go a = Just a

