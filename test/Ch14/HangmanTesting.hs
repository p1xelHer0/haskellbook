module Ch14.HangmanTesting where

import Test.QuickCheck

import Ch13.Hangman
       (fillInCharacterCorrect, fillInCharacterWrong, handleGuess)

tests :: IO ()
tests = do
  putStrLn "HangmanTesting: "
