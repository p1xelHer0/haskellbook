module Main where

import qualified Ch14.WordNumberTest as WordNumberTest
import qualified Ch14.UsingQuickCheck as UsingQuickCheck
import qualified Ch14.Idempotence as Idempotence
import qualified Ch14.HangmanTesting as Hangman
import qualified Ch14.ValidatingCiphers as Ciphers
import qualified Ch15.TestingQuickChecksPatience as Patience
import Ch15.MaybeAnotherMonoid
import qualified Ch15.Semigroup as Semigroup
import qualified Ch15.Monoid as Monoid
import qualified Ch16.InstancesOfFunc as InstancesOfFunc
import qualified Ch16.Possibly as Possibly
import qualified Ch16.ShortExercise as Either
-- import qualified Ch17.BadMonoid as BadMonoid
import qualified Ch17.ListApplicative as ListApplicative
import qualified Ch17.VariationsOnEither as VariationsOnEither
import qualified Ch17.ValidateTheInstances as ValidateTheInstances

main :: IO ()
main = do
  WordNumberTest.tests
  UsingQuickCheck.tests
  Idempotence.tests
  Hangman.tests
  Ciphers.tests
  Patience.tests
  Ch15.MaybeAnotherMonoid.tests
  Semigroup.tests
  Monoid.tests
  InstancesOfFunc.tests
  Possibly.tests
  Either.tests
  -- BadMonoid.tests
  ListApplicative.tests
  VariationsOnEither.tests
  ValidateTheInstances.tests
