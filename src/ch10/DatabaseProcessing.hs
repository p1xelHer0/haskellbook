module DatabaseProcessing where

import Data.Time
import Data.List

data DatabaseItem
  = DbString String
  | DbNumber Integer
  | DbDate UTCTime
  deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase =
  [ DbDate (UTCTime (fromGregorian 1911 5 1) (secondsToDiffTime 34123))
  , DbNumber 9001
  , DbString "Hello, world!"
  , DbDate (UTCTime (fromGregorian 1921 5 1) (secondsToDiffTime 34123))
  ]

-- 1. Write a function that filters for DbDate values and returns list of the UTCTime values inside them.
filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate xs =
  [ x
  | (DbDate x) <- xs ]

-- 2. Write a function that filters for DbNumber values and returns a list of the Integer values inside them.
filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber xs =
  [ x
  | (DbNumber x) <- xs ]

-- 3. Write a function that gets the most recent date.
mostRecent :: [DatabaseItem] -> UTCTime
mostRecent = maximum . filterDbDate

-- 4. Write a function that sums all of the DbNumber values.
sumDb :: [DatabaseItem] -> Integer
sumDb = sum . filterDbNumber

-- 5. Write a function that gets the average of the DbNumber values. You'll probably need to use fromIntegral to get from Integer to Double.
avgDb :: [DatabaseItem] -> Double
avgDb xs = (realToFrac (sumDb xs) / genericLength xs) :: Double
