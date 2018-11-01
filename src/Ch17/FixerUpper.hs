module Ch17.FixerUpper where

-- 1.
f1 = const <$> Just "Hello" <*> pure "World"

f2 = (,,,) <$> Just 90 <*> Just "Tierness" <*> pure [1, 2, 3]
