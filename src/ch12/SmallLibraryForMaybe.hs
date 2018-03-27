module SmallLibraryForMaybe where

isJust :: Maybe a -> Bool
isJust (Just a) = True
isJust Nothing = False

isNothing :: Maybe a -> Bool
isNothing a = not (isJust a)

mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee z f Nothing = z
mayybee z f (Just a) = f a

fromMaybe :: a -> Maybe a -> a
fromMaybe z Nothing = z
fromMaybe z (Just a) = a

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (x:xs) = Just x

maybeToList :: Maybe a -> [a]
maybeTolist Nothing = []
maybeToList (Just a) = [a]

catMaybes :: [Maybe a] -> [a]
catMaybes (Nothing:xs) = catMaybes xs
catMaybes (Just x:xs) = x : catMaybes xs

flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe xs = Just (go xs) where
  go (Nothing:xs) = go xs
  go (Just x:xs) = x : go xs
