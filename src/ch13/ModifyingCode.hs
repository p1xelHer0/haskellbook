module ModifyingCode where

import Data.Char (toLower)
import Control.Monad
import System.Exit (exitSuccess)

main :: IO ()
main = palindrome

palindrome :: IO ()
palindrome =
  forever $
  do line1 <- getLine
     if palindrome' line1
       then putStrLn "It's a palindrome!"
       else exitSuccess
  where
    palindrome' xs =
      map toLower (removeSpaces xs) == map toLower (reverse (removeSpaces xs))
    removeSpaces = filter (not . (`elem` " "))

type Name = String

type Age = Integer

data Person =
  Person Name
         Age
  deriving (Show)

data PersonInvalid
  = NameEmpty
  | AgeTooLow
  | PersonInvalidUnknown String
  deriving (Eq, Show)

mkPerson :: Name -> Age -> Either PersonInvalid Person
mkPerson name age
  | name /= "" && age > 0 = Right $ Person name age
  | name == "" = Left NameEmpty
  | not (age > 0) = Left AgeTooLow
  | otherwise =
    Left $
    PersonInvalidUnknown $ "Name was: " ++ show name ++ " Age was: " ++ show age

gimmiePerson :: IO ()
gimmiePerson = do
  putStrLn "Age? "
  age <- readLn :: IO Integer
  putStrLn "Name? "
  name <- getLine
  case mkPerson name age of
    Right (Person name age) ->
      putStrLn $ "Yay! Successfully got a person: " ++ name ++ ", " ++ show age
    Left (PersonInvalidUnknown s) -> putStrLn s
    Left NameEmpty -> putStrLn "Name is empty!"
    Left AgeTooLow -> putStrLn "Age is too low!"
