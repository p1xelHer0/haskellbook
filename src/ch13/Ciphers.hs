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

-- added IO
caesar :: IO String
caesar = do
  phrase <- getLine
  length <- readLn :: IO Int
  return $ map (circularChar length) phrase

-- added IO
unCaesar :: IO String
unCaesar = do
  phrase <- getLine
  length <- readLn :: IO Int
  return $ map (circularChar (length * (-1))) phrase

addCharCircular :: Char -> Char -> Char
addCharCircular c1 = circularChar (ord c1 - 97)

subtractCharCirular :: Char -> Char -> Char
subtractCharCirular c1 = circularChar ((ord c1 - 97) * (-1))

-- only works for lower case as of now :(
-- added IO
vigenere :: IO String
vigenere = do
  keyword <- getLine
  phrase <- getLine
  return $ zipWith addCharCircular (concat $ repeat keyword) phrase

-- only works for lower case as of now :(
-- added IO
unVigenere :: IO String
unVigenere = do
  keyword <- getLine
  phrase <- getLine
  return $ zipWith subtractCharCirular (concat $ repeat keyword) phrase
-- should do
-- -> vigenere "ally" "meetatdawn"
-- => "mppraeoywy"
