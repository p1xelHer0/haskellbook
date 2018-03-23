module ValidateTheWord where

newtype Word' =
  Word' String
  deriving (Eq, Show)

vowels = "aeiou"

mkWord :: String -> Maybe Word'
mkWord word = fvc word [] []
  where
    fvc [] vowels consonants =
      if length vowels > length consonants
        then Nothing
        else Just (Word' word)
    fvc (x:xs) vowels consonants =
      if x `elem` vowels
        then fvc xs (x : vowels) consonants
        else fvc xs vowels (x : consonants)
