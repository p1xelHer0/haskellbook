module EnumFromTo where

eftBool :: Bool -> Bool -> [Bool]
eftBool a b
  | a > b = []
  | a == b = [a]
  | otherwise = a : eftBool (succ a) b

eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd a b
  | a > b = []
  | a == b = [a]
  | otherwise = a : eftOrd (succ a) b

eftInt :: Int -> Int -> [Int]
eftInt a b
  | a > b = []
  | a == b = [a]
  | otherwise = a : eftInt (succ a) b

eftChar :: Char -> Char -> String
eftChar a b
  | a > b = []
  | a == b = [a]
  | otherwise = a : eftChar (succ a) b

-- generally?
eftAll
  :: (Ord a, Enum a)
  => a -> a -> [a]
eftAll a b
  | a > b = []
  | a == b = [a]
  | otherwise = a : eftAll (succ a) b
