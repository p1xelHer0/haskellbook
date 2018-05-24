module Main where

import qualified Ch14.WordNumberTest as WordNumberTest
import qualified Ch14.UsingQuickCheck as UsingQuickCheck

main :: IO ()
main = do
  WordNumberTest.tests
  UsingQuickCheck.tests
