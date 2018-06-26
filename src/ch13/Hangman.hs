module Ch13.Hangman
  ( game
  , fillInCharacterCorrect
  , fillInCharacterWrong
  , handleGuess
  ) where

import Control.Monad (forever)
import Data.Char (toLower)
import Data.Maybe
import Data.List (intersperse)
import System.Exit (exitSuccess)
import System.Random (randomRIO)

newtype WordList =
  WordList [String]
  deriving (Eq, Show)

minWordLength :: Int
minWordLength = 5

maxWordLength :: Int
maxWordLength = 9

data Puzzle =
  Puzzle String
         [Maybe Char]
         String
         String

game :: IO ()
game = do
  word <- randomWord'
  let puzzle = freshPuzzle (fmap toLower word)
  runGame puzzle

allWords :: IO WordList
allWords = do
  dict <- readFile "data/dict.txt"
  return $ WordList (lines dict)

gameWords :: IO WordList
gameWords = do
  (WordList aw) <- allWords
  return $ WordList (filter gameLength aw)
  where
    gameLength w =
      let l = length (w :: String)
      in l >= minWordLength && l < maxWordLength

randomWord :: WordList -> IO String
randomWord (WordList wl) = do
  randomIndex <- randomRIO (0, length wl - 1)
  return $ wl !! randomIndex

randomWord' :: IO String
randomWord' = gameWords >>= randomWord

instance Show Puzzle where
  show (Puzzle _ discovered guessedCorrect guessedWrong) =
    (intersperse ' ' $ fmap renderPuzzleChar discovered) ++
    " Wrong guesses so far: " ++ guessedWrong

freshPuzzle :: String -> Puzzle
freshPuzzle s = Puzzle s (map go s) [] []
  where
    go c = Nothing

charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle [] _ _ _) _ = False
charInWord (Puzzle (x:xs) a b c) char =
  (x == char) || charInWord (Puzzle xs a b c) char

alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed (Puzzle _ _ [] []) _ = False
alreadyGuessed (Puzzle a b (x:xs) []) c =
  (x == c) || alreadyGuessed (Puzzle a b xs []) c
alreadyGuessed (Puzzle a b [] (y:ys)) c =
  (y == c) || alreadyGuessed (Puzzle a b [] ys) c
alreadyGuessed (Puzzle a b (x:xs) (y:ys)) c =
  (x == c) || (y == c) || alreadyGuessed (Puzzle a b xs ys) c

renderPuzzleChar :: Maybe Char -> Char
renderPuzzleChar Nothing = '_'
renderPuzzleChar (Just c) = c

fillInCharacterCorrect :: Puzzle -> Char -> Puzzle
fillInCharacterCorrect (Puzzle word filledInSoFar guessedCorrect guessedWrong) c =
  Puzzle word newFilledInSoFar (c : guessedCorrect) guessedWrong
  where
    zipper gr wordChar guessChar =
      if wordChar == gr
        then Just wordChar
        else guessChar
    newFilledInSoFar = zipWith (zipper c) word filledInSoFar

fillInCharacterWrong :: Puzzle -> Char -> Puzzle
fillInCharacterWrong (Puzzle word filledInSoFar guessedCorrect guessedWrong) c =
  Puzzle word newFilledInSoFar guessedCorrect (c : guessedWrong)
  where
    zipper gr wordChar guessChar =
      if wordChar == gr
        then Just wordChar
        else guessChar
    newFilledInSoFar = zipWith (zipper c) word filledInSoFar

handleGuess :: Puzzle -> Char -> IO Puzzle
handleGuess puzzle guess = do
  putStrLn $ "Your guess was:" ++ [guess]
  case (charInWord puzzle guess, alreadyGuessed puzzle guess) of
    (_, True) -> do
      putStrLn
        "You already guessed that\
        \ character, pick \
        \ something else!"
      return puzzle
    (True, _) -> do
      putStrLn
        "This character was in the\
        \ word, filling the word\
        \ accordingly"
      return (fillInCharacterCorrect puzzle guess)
    (False, _) -> do
      putStrLn
        "This character wasn't in\
        \ the word, try again."
      return (fillInCharacterWrong puzzle guess)

gameOver :: Puzzle -> IO ()
gameOver (Puzzle wordToGuess _ guessedCorrect guessedWrong) =
  if length guessedWrong > 9
    then do
      putStrLn "You lose!"
      putStrLn $ "The word was: " ++ wordToGuess
      exitSuccess
    else return ()

gameWin :: Puzzle -> IO ()
gameWin (Puzzle _ filledInSoFar _ _) =
  if all isJust filledInSoFar
    then do
      putStrLn "You win!"
      exitSuccess
    else return ()

runGame :: Puzzle -> IO ()
runGame puzzle =
  forever $
  do gameWin puzzle
     gameOver puzzle
     putStrLn $ "Current puzzle is: " ++ show puzzle
     putStr "Guess a letter: "
     guess <- getLine
     case guess of
       [c] -> handleGuess puzzle c >>= runGame
       _ ->
         putStrLn
           "Your guess must\
           \ be a single character"
