module HeavyLifting where

-- 1.
a = fmap (+ 1) $ read "[1]" :: [Int]

-- 2.
b = (fmap . fmap) (++ "lol") (Just ["Hi", "Hello"])

-- 3.
c a = (* 2) $ (\x -> x - 2) a

-- 4.
d = fmap ((return '1' ++) . show) (\x -> [x,1 .. 3])

-- 5.
-- TODO
e :: IO Integer
e = let ioi = readIO "1" :: IO Integer
        changed = read ("123" ++) show ioi
    in fmap (*3) changed
