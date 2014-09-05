{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, ScopedTypeVariables #-}

import Data.Typeable
import GHC.Generics
import Test.QuickCheck
import Test.QuickCheck.Arbitrary

data Tree a = Leaf a | Branch (Tree a) (Tree a)
    deriving (Eq, Ord, Show, Generic, Typeable)

instance Arbitrary a => Arbitrary (Tree a)

t :: Tree Int
t =
  (Branch
    (Branch
      (Branch (Leaf 1) (Leaf 2))
      (Branch (Leaf 3) (Leaf 4))
    )
    (Branch
      (Branch (Leaf 5) (Leaf 6))
      (Branch (Leaf 7) (Leaf 8))
    )
  )

ls = genericShrink t

test1 = genericShrinkOrig      (0::Int) == [0]  -- true; bad, should be false
test2 = genericShrinkOrig      [0::Int] == [[]] -- true; good
test3 = genericShrinkOrigFixed (0::Int) == []   -- true; good
test4 = genericShrinkOrigFixed [0::Int] == []   -- true; bad, should be false
test5 = quickCheck $ property $ \(l :: [Int]) -> subtermsOrig l      == genericShrinkBitonic l
test6 = quickCheck $ property $ \(l :: [Int]) -> subtermsOrigFixed l == genericShrinkBitonicFixed l
