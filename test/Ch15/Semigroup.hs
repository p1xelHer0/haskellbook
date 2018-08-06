module Ch15.Semigroup where

import Data.Semigroup as S

import Test.QuickCheck
import Test.Hspec

tests :: IO ()
tests = do
  putStrLn "Semigroup:"
  quickCheck (semigroupAssoc :: TrivAssoc)
  quickCheck (semigroupAssoc :: IdentityAssoc)
  quickCheck (semigroupAssoc :: TwoAssoc)
  quickCheck (semigroupAssoc :: ThreeAssoc)
  quickCheck (semigroupAssoc :: FourAssoc)
  quickCheck (semigroupAssoc :: BoolDisjAssoc)
  quickCheck (semigroupAssoc :: BoolConjAssoc)
  quickCheck (semigroupAssoc :: OrAssoc)
  hspec $
    do describe "BoolConj" $
         do it "conjuncts: 1 <> 1 = 1" $
              BoolConj True <> BoolConj True `shouldBe` BoolConj True
            it "conjuncts: 1 <> 0 = 0" $
              BoolConj True <> BoolConj False `shouldBe` BoolConj False
       describe "BoolDisj" $
         do it "disjuncts: 1 <> 1 = 1" $
              BoolDisj True <> BoolDisj True `shouldBe` BoolDisj True
            it "disjuncts: 1 <> 0 = 1" $
              BoolDisj True <> BoolDisj False `shouldBe` BoolDisj True
       describe "Or" $
         do it "f <> s = s" $ Fst 1 <> Snd 2 `shouldBe` Snd 2
            -- we need to specify both a and b for QuickCheck to work
            it "f1 <> f2 = f2" $
              (Fst 1 :: Or Integer Integer) <> Fst 2 `shouldBe` Fst 2
            it "s <> f = s" $ Snd 1 <> Fst 2 `shouldBe` Snd 1
            -- we need to specify both a and b for QuickCheck to work
            it "s1 <> s2 = s1" $
              (Snd 1 :: Or Integer Integer) <> Snd 2 `shouldBe` Snd 1
       describe "Validation" $
         do it "s 1 <> f xs = s 1" $ Succ 1 <> Fail "blah" `shouldBe` Succ 1
            it "f1 xs <> f2 xs = xsxs" $
              (Fail "woot" :: Validation String Integer) <> Fail "blah" `shouldBe`
              Fail "wootblah"
            it "s 1 <> s 2 = s1" $
              (Succ 1 :: Validation String Integer) <> Succ 2 `shouldBe` Succ 1
            it "f xs <> s 2 = s 2" $ Fail "woot" <> Succ 2 `shouldBe` Succ 2

-- describe "Combine" $
--   do it "works yolo" $ unCombine (f <> g) $ 0 `shouldBe` Sum {getSum = 0}
semigroupAssoc
  :: (Eq m, Semigroup m)
  => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

-- 1.
data Trivial =
  Trivial
  deriving (Eq, Show)

instance Semigroup Trivial where
  _ <> _ = Trivial

instance Arbitrary Trivial where
  arbitrary = return Trivial

type TrivAssoc = Trivial -> Trivial -> Trivial -> Bool

-- 2.
newtype Identity a =
  Identity a
  deriving (Eq, Show)

instance Semigroup a =>
         Semigroup (Identity a) where
  (Identity x) <> (Identity y) = Identity $ x <> y

instance Arbitrary a =>
         Arbitrary (Identity a) where
  arbitrary = genIdentity

genIdentity
  :: Arbitrary a
  => Gen (Identity a)
genIdentity = do
  a <- arbitrary
  return (Identity a)

type IdentityAssoc = Identity String -> Identity String -> Identity String -> Bool

-- 3.
data Two a b =
  Two a
      b
  deriving (Eq, Show)

instance (Semigroup a, Semigroup b) =>
         Semigroup (Two a b) where
  (Two x1 y1) <> (Two x2 y2) = Two (x1 <> x2) (y1 <> y2)

instance (Arbitrary a, Arbitrary b) =>
         Arbitrary (Two a b) where
  arbitrary = genTwo

genTwo
  :: (Arbitrary a, Arbitrary b)
  => Gen (Two a b)
genTwo = do
  a <- arbitrary
  b <- arbitrary
  return (Two a b)

type TwoAssoc = Two String String -> Two String String -> Two String String -> Bool

-- 4.
data Three a b c =
  Three a
        b
        c
  deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c) =>
         Semigroup (Three a b c) where
  (Three x1 y1 z1) <> (Three x2 y2 z2) = Three (x1 <> x2) (y1 <> y2) (z1 <> z2)

instance (Arbitrary a, Arbitrary b, Arbitrary c) =>
         Arbitrary (Three a b c) where
  arbitrary = genThree

genThree
  :: (Arbitrary a, Arbitrary b, Arbitrary c)
  => Gen (Three a b c)
genThree = do
  a <- arbitrary
  b <- arbitrary
  c <- arbitrary
  return (Three a b c)

type ThreeAssoc = Three String String String -> Three String String String -> Three String String String -> Bool

-- 5.
data Four a b c d =
  Four a
       b
       c
       d
  deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d) =>
         Semigroup (Four a b c d) where
  (Four x1 y1 z1 t1) <> (Four x2 y2 z2 t2) =
    Four (x1 <> x2) (y1 <> y2) (z1 <> z2) (t1 <> t2)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) =>
         Arbitrary (Four a b c d) where
  arbitrary = genFour

genFour
  :: (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d)
  => Gen (Four a b c d)
genFour = do
  a <- arbitrary
  b <- arbitrary
  c <- arbitrary
  d <- arbitrary
  return (Four a b c d)

type FourAssoc = Four String String String String -> Four String String String String -> Four String String String String -> Bool

-- 6.
newtype BoolConj =
  BoolConj Bool
  deriving (Eq, Show)

instance Semigroup BoolConj where
  (BoolConj True) <> (BoolConj True) = BoolConj True
  _ <> _ = BoolConj False

instance Arbitrary BoolConj where
  arbitrary = genBoolConj

genBoolConj :: Gen BoolConj
genBoolConj = do
  a <- arbitrary
  return (BoolConj a)

type BoolConjAssoc = BoolConj -> BoolConj -> BoolConj -> Bool

-- 7.
newtype BoolDisj =
  BoolDisj Bool
  deriving (Eq, Show)

instance Semigroup BoolDisj where
  (BoolDisj True) <> _ = BoolDisj True
  _ <> (BoolDisj True) = BoolDisj True
  _ <> _ = BoolDisj False

instance Arbitrary BoolDisj where
  arbitrary = genBoolDisj

genBoolDisj :: Gen BoolDisj
genBoolDisj = do
  a <- arbitrary
  return (BoolDisj a)

type BoolDisjAssoc = BoolDisj -> BoolDisj -> BoolDisj -> Bool

-- 8.
data Or a b
  = Fst a
  | Snd b
  deriving (Eq, Show)

instance Semigroup (Or a b) where
  (Snd x) <> _ = Snd x
  _ <> y = y

instance (Arbitrary a, Arbitrary b) =>
         Arbitrary (Or a b) where
  arbitrary = genOr

genOr
  :: (Arbitrary a, Arbitrary b)
  => Gen (Or a b)
genOr = do
  a <- arbitrary
  b <- arbitrary
  frequency [(1, return (Fst a)), (1, return (Snd b))]

type OrAssoc = Or String Int -> Or String Int -> Or String Int -> Bool

-- 9.
newtype Combine a b = Combine
  { unCombine :: a -> b
  }

f = Combine $ \n -> Sum (n + 1)

g = Combine $ \n -> Sum (n - 1)

-- instance Semigroup (Combine a b) where
--   f <> g = g . f
-- 11.
data Validation a b
  = Fail a
  | Succ b
  deriving (Eq, Show)

instance Semigroup a =>
         Semigroup (Validation a b) where
  (Succ a) <> _ = Succ a
  _ <> (Succ b) = Succ b
  (Fail a) <> (Fail b) = Fail (a <> b)
