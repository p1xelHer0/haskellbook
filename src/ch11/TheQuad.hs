module TheQuad where

-- 1. Determine how many unique inhabitants each type has.
data Quad
  = One
  | Two
  | Three
  | Four
  deriving (Eq, Show)

-- how many different forms can this take?
-- 1.
-- > 8, 4 + 4
eQuad :: Either Quad Quad
eQuad = undefined

-- 2.
-- >  16, 4 * 4
prodQuad :: (Quad, Quad)
prodQuad = undefined

-- 3.
funcQuad :: Quad -> Quad
funcQuad = undefined

-- > 256, 4^4
-- 4.
-- > 8, 2 * 2 * 2
prodTBool :: (Bool, Bool, Bool)
prodTBool = undefined

-- 5.
-- > 16, (2 ^ 2) ^ 2, 2 ^ 4
gTwo :: Bool -> Bool -> Bool
gTwo = undefined

-- 6.
-- Hint: 5 digit number
-- > 65536, 2 ^ 4 ^ 4, 4 ^ 8
fTwo :: Bool -> Quad -> Quad
fTwo = undefined
