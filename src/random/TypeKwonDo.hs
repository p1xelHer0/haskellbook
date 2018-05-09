module TypeKwonDo where

-- example
data Woot

data Blah

f :: Woot -> Blah
f = undefined

g :: (Blah, Woot) -> (Blah, Blah)
g (b, w) = (b, f w)

-- 1.
f1 :: Int -> String
f1 = undefined

g1 :: String -> Int
g1 = undefined

h1 :: Int -> Char -- answer
h1 x = head $ f1 x

-- 2.
data A

data B

data C

q2 :: A -> B
q2 = undefined

w2 :: B -> C
w2 = undefined

e2 :: A -> C
-- answer
e2 x = w2 $ q2 x

-- 3.
data X

data Y

data Z

xz :: X -> Z
xz = undefined

yz :: Y -> Z
yz = undefined

xform :: (X, Y) -> (Z, Z)
-- answer
xform (x, y) = (xz x, yz y)

-- 4.
munge :: (x -> y) -> (y -> (w, z)) -> x -> w
-- answer
munge f g x = fst $ g $ f x
