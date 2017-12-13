module Data.Char where

import Data.Char

-- 1.
-- isUpper :: Char -> Bool
-- isLower :: Char -> Bool
-- 2.
filterUpperCase :: String -> String
filterUpperCase = filter isUpper

-- 3.
capitalizeFirst :: String -> String
capitalizeFirst (x:xs) = toUpper x : xs

-- 4.
capitalize :: String -> String
capitalize [] = []
capitalize (x:xs) = toUpper x : capitalize xs

-- 5 + 6.
getCapitalizedHead :: String -> Char
getCapitalizedHead = toUpper . head
