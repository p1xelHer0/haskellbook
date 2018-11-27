module Ch10.WarmUpAndReview where

-- 1.
stops = "pbtdkg"

vowels = "aeiou"

-- a)
stopVowelStop =
  [ (x, y, z)
  | x <- stops
  , y <- vowels
  , z <- stops ]

-- b)
stopVowelStopP =
  [ (x, y, z)
  | x <- stops
  , y <- vowels
  , z <- stops
  , x == 'p' ]

nouns = ["cat", "dog", "fish", "he", "she", "it", "they", "frog"]

verbs = ["eat", "run", "walk", "sneak", "rush", "laugh"]

-- c)
nounAndVerb =
  [ (a, b, c)
  | a <- nouns
  , b <- verbs
  , c <- nouns ]

-- 2. it calculates the average length of each word int the argument.
seekritFunc x = div (sum (map length (words x))) (length (words x))

-- 3.
seektritFuncFrac x =
  fromIntegral (sum (map length (words x))) / fromIntegral (length (words x))
