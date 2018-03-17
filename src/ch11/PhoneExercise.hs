module PhoneExercise where

import Data.Char
import Data.List

data Button =
  Button Digit
         String

type Digit = Char

type Presses = Int

data DaPhone =
  DaPhone [Button]

phone :: DaPhone
phone =
  DaPhone
    [ Button '1' ""
    , Button '2' "ABC"
    , Button '3' "DEF"
    , Button '4' "GHI"
    , Button '5' "JKL"
    , Button '6' "MNO"
    , Button '7' "PQRS"
    , Button '8' "TUV"
    , Button '9' "XYZ"
    , Button '*' "^"
    , Button '0' "+ _"
    , Button '#' ".,"
    ]

-- assuming the default phone definition
-- 'a' -> [('2', 1)]
-- 'A' -> [('*', 1), ('2', 1)]
reverseTaps :: DaPhone -> Char -> [(Digit, Presses)]
reverseTaps p x
  | isUpper x = ('*', 1) : reverseTaps p (toLower x)
  | otherwise = yolo p x where
    yolo (phone b:bs) x = yolo2 where
      yolo2 (button digit values) =

cellPhonesDead :: DaPhone -> String -> [(Digit, Presses)]
cellPhonesDead = undefined

convo :: [String]
convo =
  [ "Wanna play 20 questions"
  , "Ya"
  , "U 1st haha"
  , "Lol ok. Have u ever tasted alcohol"
  , "Lol ya"
  , "Wow ur cool haha. Ur turn"
  , "Ok. Do u think I am pretty Lol"
  , "Lol ya"
  , "Just making sure rofl ur turn"
  ]
