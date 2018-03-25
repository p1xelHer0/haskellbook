module SmallLibraryForMaybe where

isJust :: Maybe a -> Bool
isJust (Just a) = True
isJust Nothing = False

isNothing :: Maybe a -> Bool
isNothing a = not (isJust a)

mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee = undefined
