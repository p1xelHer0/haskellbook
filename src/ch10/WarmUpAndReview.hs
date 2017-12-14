module WarmUpAndReview where

-- 1. Given the following sets of consonants and vowels:
stops = "pbtdkg"

vowels = "aeiou"

-- a) Write a function that takes inputs from stops and vowels and makes 3-tuples of all possible stop-vowel-stop combinations. These will not all correspond to real words in English, although the stop-vowel-stop pattern is common enough that many of them will.
stopVowelStop =
  [ (x, y, z)
  | x <- stops
  , y <- vowels
  , z <- stops ]

-- b) Modify that function so that it only returns the combinations that begin with a p.
stopVowelStopP =
  [ (x, y, z)
  | x <- stops
  , y <- vowels
  , z <- stops
  , x == 'p' ]

nouns = ["cat", "dog", "fish", "he", "she", "it", "they", "frog"]

verbs = ["eat", "run", "walk", "sneak", "rush", "laugh"]

-- c) Now set up lists of nouns and verbs (instead of stops and vowels) and modify the function to make tuples representing possible noun-verb-noun sentences.
nounAndVerb =
  [ (a, b, c)
  | a <- nouns
  , b <- verbs
  , c <- nouns ]

-- 2. What does the following mystery function do? What is its type? Try to get a good sense of what it does before you test it in the REPL to verify it.
-- 2. it calculates the average length of each word int the argument.
seekritFunc x = div (sum (map length (words x))) (length (words x))

-- 3. We’d really like the answer to be more precise. Can you rewrite that using fractional division?
seektritFuncFrac x =
  fromIntegral (sum (map length (words x))) / fromIntegral (length (words x))
