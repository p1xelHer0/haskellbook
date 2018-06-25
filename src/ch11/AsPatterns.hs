module Ch11.AsPatterns
  ( capitalizeWord
  ) where

import Data.Char

f
  :: Show a
  => (a, b) -> IO (a, b)
f t@(a, _) = do
  print a
  return t

doubleUp :: [a] -> [a]
doubleUp [] = []
doubleUp xs@(x:_) = x : xs

isSubseqOf
  :: (Eq a)
  => [a] -> [a] -> Bool
isSubseqOf [] _ = True
isSubseqOf _ [] = False
isSubseqOf ax@(a:as) bx@(b:bs) = a == b && isSubseqOf as bs || isSubseqOf ax bs

capitalizeWords :: String -> [(String, String)]
capitalizeWords = map toTuple . splitAtSpace
  where
    toTuple s@(x:xs) = (s, toUpper x : xs)

-- borrowing this from an older chapter
splitAtSpace :: String -> [String]
splitAtSpace [] = []
splitAtSpace (' ':s) = splitAtSpace s
splitAtSpace s = takeWhile (/= ' ') s : splitAtSpace (dropWhile (/= ' ') s)

capitalizeWord :: String -> String
capitalizeWord (x:xs) = toUpper x : xs

splitAtPeriod :: String -> [String]
splitAtPeriod [] = []
splitAtPeriod ('.':s) = "." : splitAtPeriod s
splitAtPeriod s = takeWhile (/= '.') s : splitAtPeriod (dropWhile (/= '.') s)

splitSentence :: String -> [String]
splitSentence = map capitalizeFirstNonWhitespace . splitAtPeriod
  where
    capitalizeFirstNonWhitespace (' ':xs) = ' ' : capitalizeFirstNonWhitespace xs
    capitalizeFirstNonWhitespace (x:xs) = toUpper x : xs

capitalizeParagraph :: String -> String
capitalizeParagraph sentence = concat (splitSentence sentence)
