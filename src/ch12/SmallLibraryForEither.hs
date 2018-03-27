module SmallLibraryForEither where

lefts' :: [Either a b] -> [a]
lefts' (Right x:xs) = lefts' xs
lefts' (Left x:xs) = x : lefts' xs

-- something something
lefts'' :: [Either a b] -> [a]
lefts'' = foldr go [] where
  go (Right x:xs) = go xs
  go (Left x:xs) = x : go xs
