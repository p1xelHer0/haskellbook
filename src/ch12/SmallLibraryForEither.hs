module SmallLibraryForEither where

-- first attempt
lefts' :: [Either a b] -> [a]
lefts' (Left x:xs) = x : lefts' xs
lefts' (Right x:xs) = lefts' xs
lefts' _ = []

lefts'' :: [Either a b] -> [a]
lefts'' = foldr left [] where
  left (Left x) xs = x : xs
  left (Right _) xs = xs

-- first attempt
rights' :: [Either a b] -> [b]
rights' (Left x:xs) = rights' xs
rights' (Right x:xs) = x : rights' xs
rights' _ = []

rights'' :: [Either a b] -> [b]
rights'' = foldr right [] where
  right (Left _) xs = xs
  right (Right x) xs = x : xs

partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' xs = (lefts' xs, rights' xs)

eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' _ (Left _) = Nothing
eitherMaybe' f (Right x) = Just (f x)

either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' f _ (Left a) = f a
either' _ g (Right b) = g b

eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' x = either' (const Nothing) (Just . x)

