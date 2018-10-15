module FixerUpper where

-- 1.
a = const <$> Just "Hello" <*> pure "World"

b = (,,,) <$> Just 90 <*> Just 10 <*> Just "Tireness" <*> pure [1, 2, 3]
