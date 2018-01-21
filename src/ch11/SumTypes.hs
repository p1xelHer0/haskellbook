module SumTypes where

import Data.Int

-- 1. Given a datatype
data BigSmall
  = Big Bool
  | Small Bool
  deriving (Eq, Show)

-- What is the cardinality of this datatype? Hint: We already know Bool’s cardinality. Show your work as demonstrated earlier.
-- -> 4. 1 + 1 + 1 + 1
-- 2. Given a datatype
data NumberOrBool
  = Numba Int8
  | BoolyBool Bool
  deriving (Eq, Show)
-- -> 258. 256 + 1 + 1
myNumba = Numba (-128)
