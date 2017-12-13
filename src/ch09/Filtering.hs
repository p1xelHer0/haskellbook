module Filtering where

-- 1.
multiplesOfThree = filter (\x -> x `mod` 3 == 0) [1 .. 30]

-- 2.
howManyMultiplesOfThree = length multiplesOfThree

-- 3.
article :: String -> Bool
article "the" = True
article "an" = True
article "a" = True
article _ = False

myFilter :: String -> [String]
myFilter = filter (not . article) . myWords
