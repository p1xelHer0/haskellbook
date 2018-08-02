module Ch14.Idempotence where

import Test.QuickCheck
import Data.List (sort)
import Ch11.AsPatterns (capitalizeWord)

tests :: IO ()
tests = do
  putStrLn "Idempotence: "
  quickCheck f
  quickCheck f'

twice f = f . f

fourTimes = twice . twice

-- 1.
f :: String -> Bool
f x =
  (capitalizeWord x == twice capitalizeWord x) &&
  (capitalizeWord x == fourTimes capitalizeWord x)

-- 2.
f' :: [Int] -> Bool
f' x = (sort x == twice sort x) && (sort x == fourTimes sort x)
