module CH6Exercises where

import Data.List

-- Multiple choice {{{
-- 1.
-- The Eq class
-- a) includes all types in Haskell
-- nope
-- b) is the same as the Ord class
-- nope
-- c) makes equality tests possible
-- yes
-- d) only includes numeric types
-- no, you can compare Char for example
-- The typeclass Ord
-- a) allows any two values to be compared
-- nope
-- b) is a subclass of Eq
-- checking if two things are equal is a necessity to
-- even be able to see if something is greater or smaller than
-- something else
-- therefore c)
-- c) is a superclass of Eq
-- is incorrect
-- d) has no instance for Bool
-- 3.
-- Ord :: Ord =>  a -> a -> Bool
-- > is an infix operater which compares two things
-- that CAN be compared, therefore they both need the Typeclass Ord
-- 4.
-- I don't understand this, In?
-- 5.
-- Int and Integer numbers, because:
-- fractional numbers are not integral numbers
-- cats
-- positive or negative has nothing to do with it
-- }}}
-- Does it typecheck? {{{
-- example.
-- x :: Int -> Int
-- x blah = blah + 20
-- this gives an Error because GHC can't find an implementation of
-- the typeclass Show for the type Int -> Int
-- printIt :: IO ()
-- printIt = putStrLn (show x)
-- 1.
-- data Person = Person Bool
data Person =
  Person Bool
  deriving (Show)

printPerson :: Person -> IO ()
printPerson person = putStrLn (show person)

-- Person has no instance of Show, derive it
-- 2.
data Mood
  = Blah
  | Woot
  deriving (Show, Eq)

-- deriving Show
settleDown x =
  if x == Woot
    then Blah
    else x

-- we can't compare Woot with x if it doesn't derive Eq
-- 3.
-- a) inputs that can be compared to Mood, for example another Mood, like Bla
-- b) Num can't be compared to Mood
-- c) It should not run, Mood has an instance of Eq, not Ord
-- 4.
type Subject = String

type Verb = String

type Object = String

data Sentence =
  Sentence Subject
           Verb
           Object
  deriving (Eq, Show)

-- this expects a third String, no?
s1 = Sentence "dogs" "drool"

-- this looks good
s2 = Sentence "Julie" "loves" "dogs"

-- after futher inspection with the REPL it seems that
-- s1 :: Object -> Sentence
-- where as
-- s2 :: Sentence
--
-- also, s1 can't be printed in the REPL!s2 can,
-- because the Sentence has an instance on Show,
-- whereas the function Object -> Sentence does not
-- this is because we have partially applied the Sentence,
-- s1 is now a function which expects a single String
-- s1 "yolo" -> "dogs" "drool" "yolo"
-- }}}
-- Given a datatype declaration, what can we do? {{{
data Rocks =
  Rocks String
  deriving (Eq, Show)

data Yeah =
  Yeah Bool
  deriving (Eq, Show)

data Papu =
  Papu Rocks
       Yeah
  deriving (Eq, Show)

-- 1.
-- phew = Papu "chases" True
phew = Papu (Rocks "chases") (Yeah True)

-- we need to pass an instace of Rocks and Yeah, not just the types the expect
-- 2.
truth = Papu (Rocks "chomskydoz") (Yeah True)

-- this works, according to the first example and explaination
-- 3.
equalityForAll :: Papu -> Papu -> Bool
equalityForAll p p' = p == p'

-- Papu dervices Eq so this should work
-- 3.
-- comparePapus :: Papu -> Papu -> Bool
-- comparePapus p p' = p > p'
-- Papu does not derive Ord, so this sould not work!
-- }}}
-- Match the types {{{
-- 1.
-- a)
i
  :: Num a
  => a
i = 1

-- b)
-- i :: a
-- what is this even, a? nope
-- 2.
-- a)
f2 :: Float
f2 = 1.0

-- b)
-- f :: Num a => a
-- Num should not be able to handle decimals? Fractional is the first
-- typeclass that has that kind of implementation
-- 3.
-- a)
f3 :: Float
f3 = 1.0

-- b)
-- f :: Fractional a => a
-- Float is a subclass of Fractional so this should work
-- 4.
-- a)
f4 :: Float
f4 = 1.0

-- b)
-- f :: RealFrac a => a
-- RealFrac is a subclass of Fractional so this should work
-- 5.
-- a)
freud5 :: a -> a
freud5 x = x

-- freud :: Ord a => a -> a
-- this should work just fine, we are just adding a constraint to a
-- 6.
-- a)
freud6 :: a -> a
freud6 x = x

-- freud :: Int -> Int
-- this should work just fine, we are just adding a constraint to a
-- 7.
-- a)
myX7 = 1 :: Int

sigmund7 :: Int -> Int
sigmund7 x = myX7

-- sigmund7 :: a -> a
-- this would not work since the function always returns myX, which is an Int. I guess this would work though:
-- sigmund7 :: a -> Int
-- 8.
-- a)
myX8 = 1 :: Int

sigmund8 :: Int -> Int
sigmund8 x = myX8

-- sigmund8 :: Num a => a -> a
-- Int is a instance of Num, "moving upwards"
-- in the Typeclass hierarchy is never good!
-- Int has definitions that needs to be forfilled,
-- which Num does not!
-- 9.
-- a)
jung
  :: Ord a
  => [a] -> a
jung xs = head (sort xs)

-- b)
-- jung :: [Int] -> Int
-- Int implements the Typeclass Ord so this should work
-- 10.
-- a)
young :: [Char] -> Char
young xs = head (sort xs)

-- b)
-- young :: Ord a => [a] -> a
-- this should work since both I supposed sort only needs a
-- Ord typeclass to be able to sort
-- 11.
-- a)
mySort :: [Char] -> [Char]
mySort = sort

signifier :: [Char] -> Char
signifier xs = head (mySort xs)

-- b)
-- signifier :: Ord a => [a] -> a
-- pretty much the same as above?
-- }}}
-- Type-Kwon-Do Two: Electric Typealoo {{{
-- 1.
chk
  :: Eq b
  => (a -> b) -> a -> b -> Bool
chk a b = first (a, b) == last (a, b)

-- 2.
arith :: Num b -> (a -> b) -> Integer -> a -> b
arith a b = null
-- }}}
