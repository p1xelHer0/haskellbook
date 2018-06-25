module Main where

import qualified Ch14.WordNumberTest as WordNumberTest
import qualified Ch14.UsingQuickCheck as UsingQuickCheck
import qualified Ch14.Idempotence as Idempotence

main :: IO ()
main = do
  WordNumberTest.tests
  UsingQuickCheck.tests
  Idempotence.tests
