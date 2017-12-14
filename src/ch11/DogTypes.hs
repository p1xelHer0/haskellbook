module DogTypes where

data PugType =
  PugData

data HuskyType a =
  HuskyData

data DogueDeBordeaux doge =
  DogueDeBordeaux doge

myPug = PugData :: PugType

myHusky :: HuskyType a
myHusky = HuskyData

myOtherHusky
  :: Num a
  => HuskyType a
myOtherHusky = HuskyData

myOtherOtherHusky :: HuskyType [[[[Int]]]]
myOtherOtherHusky = HuskyData

myDoge :: DogueDeBordeaux Int
myDoge = DogueDeBordeaux 10

data Doggies a
  = Husky a
  | Mastiff a
  deriving (Eq, Show)
-- 1. Is Doggies a type constructor or a data constructor?
-- -- type constructor.
-- 2. What is the kind of Doggies?
-- -- :k Doggies :: * -> *
-- 3. What is the kind of Doggies String?
-- -- :k Doggies String :: *
-- 4. What is the type of Husky 10?
-- -- :t Husky 10 :: Num => a -> Doggies a
-- 5. What is the type of Husky (10 :: Integer)?
-- -- :t Husky (10 :: Integer) :: Doggies Integer
-- 6. What is the type of Mastiff "Scooby Doo"?
-- -- :t Mastiff "Scooby Doo" :: Doggies String
-- 7. Is DogueDeBordeaux a type constructor or a data constructor?
-- -- both
-- 8. What is the type of DogueDeBordeaux?
-- -- :t DogueDeBordeaux :: doge -> DogueDeBordeaux doge
-- 9. What is the type of DogueDeBordeaux "doggie!"
-- -- :t DogueDeBordeaux "doggie!" :: DogueDeBordeaux String
