module ForExmaple where

data Example = MakeExample deriving Show

data LikeExample = MakeLikeExample Int deriving (Show, Eq)

-- 1. You can query the type of a value in GHCi with the :type command, also abbreviated :t.
-- What is the type of data constructor MakeExample? What happens when you request the type of Example?
-- > :t Example = MakeExample :: Example, since the MakeExample is just the constructor, which `constructs` the type, which is on the lefthand side of the `=`. The actual type is still `Example`
-- > `Data constructor not in scope: Example`
-- 2. What if you try :info on Example in GHCi? Can you determine what typeclass instances are defined for the Example type using :info in GHCi?
-- ->
-- data LikeExample = MakeLikeExample Int
        -- Defined at src/ch11/ForExample.hs:5:1
-- instance [safe] Eq LikeExample
  -- Defined at src/ch11/ForExample.hs:5:56
-- instance [safe] Show LikeExample
  -- Defined at src/ch11/ForExample.hs:5:50
-- 3. Try making a new datatype like Example but with a single type argument added to MakeExample, such as Int. What has changed when you query MakeExample with :type in GHCi
-- ->
-- data LikeExample = MakeLikeExample Int
        -- Defined at src/ch11/ForExample.hs:5:1
-- instance [safe] Eq LikeExample
  -- Defined at src/ch11/ForExample.hs:5:56
-- instance [safe] Show LikeExample
  -- Defined at src/ch11/ForExample.hs:5:50
