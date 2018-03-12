module Ciphers where

import Data.Char

-- Original Caesar cipher from ch9 below
-- this could probably be much better,
-- but this is my first working solution!
circularUpper :: Int -> Char -> Int
circularUpper n c
  | ord c + mod n 26 > ord 'Z' = ord 'A' + mod (ord c + mod n 26) (ord 'Z') - 1
  | otherwise = ord c + mod n 26

circularLower :: Int -> Char -> Int
circularLower n c
  | ord c + mod n 26 > ord 'z' = ord 'a' + mod (ord c + mod n 26) (ord 'z') - 1
  | otherwise = ord c + mod n 26

circularShift :: Int -> Char -> Int
circularShift n c
  | isUpper c = circularUpper n c
  | isLower c = circularLower n c

circularChar :: Int -> Char -> Char
circularChar n c = chr (circularShift n c)

caesar :: Int -> String -> String
caesar n = map (circularChar n)

unCaesar :: Int -> String -> String
unCaesar n = caesar (-n)
