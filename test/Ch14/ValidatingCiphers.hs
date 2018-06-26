module Ch14.ValidatingCiphers where

import Test.QuickCheck
import Ch13.Ciphers (caesar, vigenere)

tests :: IO ()
tests = do
  putStrLn "ValidatingCiphers: "
