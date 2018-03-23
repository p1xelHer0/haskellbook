module StringProcessing where

splitWords :: String -> [String]
splitWords [] = []
splitWords (' ':s) = splitWords s
splitWords s = takeWhile (/= ' ') s : splitWords (dropWhile (/= ' ') s)

splitWordsWithSpace :: String -> [String]
splitWordsWithSpace [] = []
splitWordsWithSpace (' ':s) = " " : splitWordsWithSpace s
splitWordsWithSpace s =
  takeWhile (/= ' ') s : splitWordsWithSpace (dropWhile (/= ' ') s)

substring :: String -> String -> Bool
substring xs [] = False
substring xs ys
  | prefix xs ys = True
  | substring xs (tail ys) = True
  | otherwise = False

prefix :: String -> String -> Bool
prefix [] ys = True
prefix (x:xs) [] = False
prefix (x:xs) (y:ys) = (x == y) && prefix xs ys

-- >>> notThe "the"
-- Nothing
-- >>> notThe "blahtheblah"
-- Just "blahtheblah"
-- >>> notThe "woot"
-- Just "woot"
notThe :: String -> Maybe String
notThe xs =
  if substring "the" xs
    then Nothing
    else Just xs

-- 1.
replaceThe :: String -> String
replaceThe [] = []
replaceThe xs = concatMap (theToA . notThe) $ splitWordsWithSpace xs
  where
    theToA Nothing = "a"
    theToA (Just xs) = xs

-- 2.
countTheBeforeVowels' :: [String] -> Int
countTheBeforeVowels' xss = go xss 0
  where
    go [_] acc = acc
    go (a:b@(bh:_):c) acc =
      if a == "the" && bh `elem` "aeiou"
        then go (b : c) (acc + 1)
        else go (b : c) acc

countTheBeforeVowels :: String -> Int
countTheBeforeVowels [] = 0
countTheBeforeVowels xs = countTheBeforeVowels' (splitWords xs)

-- 3.
countVowels :: String -> Integer
countVowels xs = fromIntegral (length (vowels' xs []))
  where
    vowels' [] foundVowels = foundVowels
    vowels' (x:xs) foundVowels =
      if x `elem` "aeiou"
        then vowels' xs (x : foundVowels)
        else vowels' xs foundVowels
