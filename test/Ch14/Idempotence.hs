module Ch14.Idempotence where

import Test.QuickCheck
import Ch11.AsPatterns (capitalizeWord)

tests :: IO ()
tests = putStrLn "yo"

twice f = f . f

fourTimes = twice . twice
