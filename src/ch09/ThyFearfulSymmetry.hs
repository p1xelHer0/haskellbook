module ThyFearfulSymmetry where

-- 1.
myWords :: String -> [String]
myWords [] = []
myWords (' ':s) = myWords s
myWords s = takeWhile (/= ' ') s : myWords (dropWhile (/= ' ') s)

-- 2.
firstSen = "Tyger Tyger, burning bright\n"

secondSen = "In the forests of the night\n"

thirdSen = "What immortal hand or eye\n"

fourthSen = "Could frame thy fearful symmetry?"

sentences = firstSen ++ secondSen ++ thirdSen ++ fourthSen

-- putStrLn sentences -- should print
-- Tyger Tyger, burning bright
-- In the forests of the night
-- What immortal hand or eye
-- Could frame thy fearful symmetry?
-- Implement this
myLines :: String -> [String]
myLines [] = []
myLines ('\n':s) = myLines s
myLines s = takeWhile (/= '\n') s : myLines (dropWhile (/= '\n') s)

shouldEqual =
  [ "Tyger Tyger, burning bright"
  , "In the forests of the night"
  , "What immortal hand or eye"
  , "Could frame thy fearful symmetry?"
  ]

main :: IO ()
main = print $ "Are they equal? " ++ show (myLines sentences == shouldEqual)

-- 3.
mySplit :: Char -> String -> [String]
mySplit _ [] = []
mySplit c s@(x:xs) =
  if c == x
    then mySplit c xs
    else takeWhile (/= c) s : mySplit c (dropWhile (/= c) s)
