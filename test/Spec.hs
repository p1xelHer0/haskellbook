module Main where

import qualified Ch14.WordNumberTest as WordNumberTest
import qualified Ch14.UsingQuickCheck as UsingQuickCheck
import qualified Ch14.Idempotence as Idempotence
import qualified Ch14.HangmanTesting as Hangman
import qualified Ch14.ValidatingCiphers as Ciphers
import qualified Ch15.TestingQuickChecksPatience as Patience
import qualified Ch15.MaybeAnotherMonoid as Monoid
import qualified Ch15.Semigroup as Semigroup

main :: IO ()
main = do
  WordNumberTest.tests
  UsingQuickCheck.tests
  Idempotence.tests
  Hangman.tests
  Ciphers.tests
  Patience.tests
  Monoid.tests
  Semigroup.tests
