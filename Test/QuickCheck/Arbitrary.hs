-- | Type classes for random generation of values.
{-# LANGUAGE CPP #-}
#ifndef NO_GENERICS
{-# LANGUAGE DefaultSignatures, FlexibleContexts, TypeOperators #-}
{-# LANGUAGE FlexibleInstances, KindSignatures, ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses, OverlappingInstances #-}
#endif
#ifndef NO_SAFE_HASKELL
{-# LANGUAGE Safe #-}
#endif
module Test.QuickCheck.Arbitrary
  (
  -- * Arbitrary and CoArbitrary classes
    Arbitrary(..)
  , CoArbitrary(..)

  -- ** Helper functions for implementing arbitrary
  , arbitrarySizedIntegral        -- :: Integral a => Gen a
  , arbitraryBoundedIntegral      -- :: (Bounded a, Integral a) => Gen a
  , arbitrarySizedBoundedIntegral -- :: (Bounded a, Integral a) => Gen a
  , arbitrarySizedFractional      -- :: Fractional a => Gen a
  , arbitraryBoundedRandom        -- :: (Bounded a, Random a) => Gen a
  , arbitraryBoundedEnum          -- :: (Bounded a, Enum a) => Gen a
  -- ** Helper functions for implementing shrink
#ifndef NO_GENERICS
  , genericArbitrary   -- :: (Generic a, GArbitrary (Rep a)) => Gen a
  , genericShrink      -- :: (Generic a, Typeable a, RecursivelyShrink (Rep a), Subterms (Rep a)) => a -> [a]
  , genericShrinkOrig
  , subtermsOrig
  , genericShrinkOrigFixed
  , subtermsOrigFixed
  , genericShrinkBitonic
  , genericShrinkBitonicFixed
  , subterms           -- :: (Generic a, Subterms (Rep a)) => a -> [a]
  , subtermsIncl
  , recursivelyShrink  -- :: (Generic a, RecursivelyShrink (Rep a)) => a -> [a]
#endif
  , shrinkNothing            -- :: a -> [a]
  , shrinkList               -- :: (a -> [a]) -> [a] -> [[a]]
  , shrinkIntegral           -- :: Integral a => a -> [a]
  , shrinkRealFrac           -- :: RealFrac a => a -> [a]
  , shrinkRealFracToInteger  -- :: RealFrac a => a -> [a]
  -- ** Helper functions for implementing coarbitrary
  , coarbitraryIntegral      -- :: Integral a => a -> Gen b -> Gen b
  , coarbitraryReal          -- :: Real a => a -> Gen b -> Gen b
  , coarbitraryShow          -- :: Show a => a -> Gen b -> Gen b
  , coarbitraryEnum          -- :: Enum a => a -> Gen b -> Gen b
  , (><)

  -- ** Generators which use arbitrary
  , vector       -- :: Arbitrary a => Int -> Gen [a]
  , orderedList  -- :: (Ord a, Arbitrary a) => Gen [a]
  , infiniteList -- :: Arbitrary a => Gen [a]
  )
 where

--------------------------------------------------------------------------
-- imports

import Control.Applicative
import System.Random(Random)
import Test.QuickCheck.Gen
import Test.QuickCheck.Gen.Unsafe

{-
import Data.Generics
  ( (:*:)(..)
  , (:+:)(..)
  , Unit(..)
  )
-}

import Data.Char
  ( chr
  , ord
  , isLower
  , isUpper
  , toLower
  , isDigit
  , isSpace
  )

#ifndef NO_FIXED
import Data.Fixed
  ( Fixed
  , HasResolution
  )
#endif

import Data.Ratio
  ( Ratio
  , (%)
  , numerator
  , denominator
  )

import Data.Complex
  ( Complex((:+)) )

import Data.List
  ( sort
  , nub
  )

import Control.Monad
  ( liftM
  , liftM2
  , liftM3
  , liftM4
  , liftM5
  )

import Data.Int(Int8, Int16, Int32, Int64)
import Data.Word(Word, Word8, Word16, Word32, Word64)

#ifndef NO_GENERICS
import GHC.Generics
import Data.Typeable
#endif

--------------------------------------------------------------------------
-- ** class Arbitrary

#ifndef NO_GENERICS
-- | Random generation and shrinking of values.
--
-- A default `arbitrary` definition is provided using "GHC.Generics".
-- If your data type is an instance of `Generic`, you can simply write:
--
-- >instance Arbitrary Mydatatype
--
-- Polymorphic example:
--
-- >data Tree a = Leaf a | Branch (Tree a) (Tree a)
-- >    deriving (Generic)
-- >
-- >instance Arbitrary a => Arbitrary (Tree a)
--
-- There is no default `shrink` since we want to keep the empty list default,
-- but a `genericShrink` is provided.
#else
-- | Random generation and shrinking of values.
#endif
class Arbitrary a where
  -- | A generator for values of the given type.
  arbitrary :: Gen a
#ifndef NO_GENERICS
  default arbitrary :: (Generic a, GArbitrary (Rep a)) => Gen a
  arbitrary = genericArbitrary
#else
  arbitrary = error "no default generator"
#endif

  -- | Produces a (possibly) empty list of all the possible
  -- immediate shrinks of the given value. The default implementation
  -- returns the empty list, so will not try to shrink the value.
  --
  -- Most implementations of 'shrink' should try at least three things:
  --
  -- 1. Shrink a term to any of its immediate subterms.
  --
  -- 2. Recursively apply 'shrink' to all immediate subterms.
  --
  -- 3. Type-specific shrinkings such as replacing a constructor by a
  --    simpler constructor.
  --
  -- For example, suppose we have the following implementation of binary trees:
  --
  -- > data Tree a = Nil | Branch a (Tree a) (Tree a)
  --
  -- We can then define 'shrink' as follows:
  --
  -- > shrink Nil = []
  -- > shrink (Branch x l r) =
  -- >   -- shrink Branch to Nil
  -- >   [Nil] ++
  -- >   -- shrink to subterms
  -- >   [l, r] ++
  -- >   -- recursively shrink subterms
  -- >   [Branch x' l' r' | (x', l', r') <- shrink (x, l, r)]
  --
  -- There are a couple of subtleties here:
  --
  -- * QuickCheck tries the shrinking candidates in the order they
  --   appear in the list, so we put more aggressive shrinking steps
  --   (such as replacing the whole tree by @Nil@) before smaller
  --   ones (such as recursively shrinking the subtrees).
  --
  -- * It is tempting to write the last line as
  --   @[Branch x' l' r' | x' <- shrink x, l' <- shrink l, r' <- shrink r]@
  --   but this is the /wrong thing/! It will force QuickCheck to shrink
  --   @x@, @l@ and @r@ in tandem, and shrinking will stop once /one/ of
  --   the three is fully shrunk.
  --
  -- There is a fair bit of boilerplate in the code above.
  -- We can avoid it with the help of some generic functions;
  -- note that these only work on GHC 7.2 and above.
  -- The function 'genericShrink' tries shrinking a term to all of its
  -- subterms and, failing that, recursively shrinks the subterms.
  -- Using it, we can define 'shrink' as:
  --
  -- > shrink x = shrinkToNil x ++ genericShrink x
  -- >   where
  -- >     shrinkToNil Nil = []
  -- >     shrinkToNil (Branch _ l r) = [Nil]
  --
  -- 'genericShrink' is a combination of 'subterms', which shrinks
  -- a term to any of its subterms, and 'recursivelyShrink', which shrinks
  -- all subterms of a term. These may be useful if you need a bit more
  -- control over shrinking than 'genericShrink' gives you.
  --
  -- A final gotcha: we cannot define 'shrink' as simply @'shrink' x = Nil:'genericShrink' x@
  -- as this shrinks @Nil@ to @Nil@, and shrinking will go into an
  -- infinite loop.
  --
  -- If all this leaves you bewildered, you might try @'shrink' = 'genericShrink'@ to begin with,
  -- after deriving @Generic@ and @Typeable@ for your type. However, if your data type has any
  -- special invariants, you will need to check that 'genericShrink' can't break those invariants.
  shrink :: a -> [a]
  shrink _ = []

#ifndef NO_GENERICS


newtype Tagged2 (s :: * -> *) b = Tagged2 {unTagged2 :: b}


-- | Calculates the size of a sum type (numbers of alternatives).
--
-- Example: `data X = A | B | C` has `sumSize` 3.
class SumSize f where
  sumSize :: Tagged2 f Int

-- Recursive case: Sum split `(:+:)`..
instance (SumSize a, SumSize b) => SumSize (a :+: b) where
  sumSize = Tagged2 $ unTagged2 (sumSize :: Tagged2 a Int) +
                          unTagged2 (sumSize :: Tagged2 b Int)
  {-# INLINE sumSize #-}

-- Constructor base case.
instance SumSize (C1 s a) where
  sumSize = Tagged2 1
  {-# INLINE sumSize #-}


-- | This class takes an integer `x` and returns a `gArbitrary` value
-- for the `x`'th alternative in a sum type.
class ChooseSum f where
  chooseSum :: Int -> Gen (f a)

-- Recursive case: Check whether `x` lies in the left or the right side
-- of the (:+:) split.
instance ( GArbitrary a, GArbitrary b
         , SumSize    a, SumSize    b
         , ChooseSum  a, ChooseSum  b ) => ChooseSum (a :+: b) where
  chooseSum x = do
    let sizeL = unTagged2 (sumSize :: Tagged2 a Int)
    if x <= sizeL
      then L1 <$> chooseSum x
      else R1 <$> chooseSum (x - sizeL)

-- Constructor base case.
instance (GArbitrary a) => ChooseSum (C1 s a) where
  chooseSum 1 = gArbitrary
  chooseSum _ = error "chooseSum: BUG"


class GArbitrary f where
  gArbitrary :: Gen (f a)

instance GArbitrary U1 where
  gArbitrary = return U1

instance (GArbitrary a, GArbitrary b) => GArbitrary (a :*: b) where
  gArbitrary = (:*:) <$> gArbitrary <*> gArbitrary

instance ( GArbitrary a, GArbitrary b
         , SumSize    a, SumSize    b
         , ChooseSum  a, ChooseSum  b ) => GArbitrary (a :+: b) where
  gArbitrary = do
    -- We cannot simply choose with equal probability between the left and
    -- right part of the `a :+: b` (e.g. with `choose (False, True)`),
    -- because GHC.Generics does not guarantee :+: to be balanced; even if it
    -- did, it could only do so for sum types with 2^n alternatives.
    -- If we did that and got a data structure of form `(a :+: (b :+: c))`,
    -- then a would be chosen just as often as b and c together.
    -- So we first have to compute the number of alternatives using `sumSize`,
    -- and then uniformly sample a number in the corresponding range.
    let size = unTagged2 (sumSize :: Tagged2 (a :+: b) Int)
    x <- choose (1, size)
    -- Optimisation:
    -- We could just recursively call `gArbitrary` on the left orright branch
    -- here, as in
    --   if x <= sizeL
    --     then L1 <$> gArbitrary
    --     else R1 <$> gArbitrary
    -- but this would unnecessarily sample again in the same sum type, and that
    -- even though `x` completely determines which alternative to choose,
    -- and sampling is slow because it needs IO and random numbers.
    -- So instead we use `chooseSum x` to pick the x'th alternative from the
    -- current sum type.
    -- This made it around 50% faster for a sum type with 26 alternatives
    -- on my computer.
    chooseSum x

instance GArbitrary a => GArbitrary (M1 i c a) where
  gArbitrary = M1 <$> gArbitrary

instance Arbitrary a => GArbitrary (K1 i a) where
  gArbitrary = K1 <$> arbitrary


-- | `Gen` for generic instances in which each constructor has equal probability
-- of being chosen.
genericArbitrary :: (Generic a, GArbitrary (Rep a)) => Gen a
genericArbitrary = to <$> gArbitrary


-- TODO check if using Rec1 can improve finding the recursive case
--      so that we don't have to use MultiParamTypeClasses and OverlappingInstances


-- | Shrink a term to any of its immediate subterms,
-- and also recursively shrink all subterms.
genericShrink :: (Generic a, Typeable a, Arbitrary a, RecursivelyShrink (Rep a), GSubterms (Rep a) a) => a -> [a]
genericShrink x = subterms x ++ recursivelyShrink x


-- Problem description
--
-- Problem: the original genericShrink (genericShrinkOrig) gave: genericShrinkOrig (0::Int) == [0]; that's bad
--       The reason is that subtermsOrig (0::Int) == [0], the 0 should be a subterm
-- Also: Is it correct to allow users to call `genericShrink`?
--       They could accidentally call it on 1::Int, which would be wrong
--       since 1 has neither subterms nor recursively shrinkable subterms.
-- Idea: The 1->0 shrink is a
--         "Type-specific shrinking such as replacing a constructor by a simpler constructor"
--       and thus not covered by subterms or grecursivelyShrink
-- Update: I think I fixed this now with my GSubterms


-- `recursivelyShrink` is and has always been correct.

-- | Recursively shrink all immediate subterms.
recursivelyShrink :: (Generic a, RecursivelyShrink (Rep a)) => a -> [a]
recursivelyShrink = map to . grecursivelyShrink . from

class RecursivelyShrink f where
  grecursivelyShrink :: f a -> [f a]

instance (RecursivelyShrink f, RecursivelyShrink g) => RecursivelyShrink (f :*: g) where
  grecursivelyShrink (x :*: y) =
    [x' :*: y | x' <- grecursivelyShrink x] ++
    [x :*: y' | y' <- grecursivelyShrink y]

instance (RecursivelyShrink f, RecursivelyShrink g) => RecursivelyShrink (f :+: g) where
  grecursivelyShrink (L1 x) = map L1 (grecursivelyShrink x)
  grecursivelyShrink (R1 x) = map R1 (grecursivelyShrink x)

instance RecursivelyShrink f => RecursivelyShrink (M1 i c f) where
  grecursivelyShrink (M1 x) = map M1 (grecursivelyShrink x)

instance Arbitrary a => RecursivelyShrink (K1 i a) where
  grecursivelyShrink (K1 x) = map K1 (shrink x)

instance RecursivelyShrink U1 where
  grecursivelyShrink U1 = []

-- TODO check if we should work on V1 as well.


-- The following is wrong because genericShrinkOrig (0::Int) == [0].

genericShrinkOrig :: (Generic a, Typeable a, RecursivelyShrink (Rep a), SubtermsOrig (Rep a)) => a -> [a]
genericShrinkOrig x = subtermsOrig x ++ recursivelyShrink x

-- | All immediate subterms of a term.
subtermsOrig :: (Generic a, Typeable a, SubtermsOrig (Rep a)) => a -> [a]
subtermsOrig = gsubtermsOrig . from

class SubtermsOrig f where
  gsubtermsOrig :: Typeable b => f a -> [b]

instance (SubtermsOrig f, SubtermsOrig g) => SubtermsOrig (f :*: g) where
  gsubtermsOrig (x :*: y) =
    gsubtermsOrig x ++ gsubtermsOrig y

instance (SubtermsOrig f, SubtermsOrig g) => SubtermsOrig (f :+: g) where
  gsubtermsOrig (L1 x) = gsubtermsOrig x
  gsubtermsOrig (R1 x) = gsubtermsOrig x

instance SubtermsOrig f => SubtermsOrig (M1 i c f) where
  gsubtermsOrig (M1 x) = gsubtermsOrig x

instance Typeable a => SubtermsOrig (K1 i a) where
  gsubtermsOrig (K1 x) =
    case cast x of
      Nothing -> []
      Just y -> [y] -- Bug: this will shrink 0 to [0]

instance SubtermsOrig U1 where
  gsubtermsOrig U1 = []



-- Same as genericShrinkOrig, but replaced the K1 instance from `[y]` to `shrink y`.
-- The following is wrong because genericShrinkOrigFixed [0::Int] == [] (should be [[]])

genericShrinkOrigFixed :: (Arbitrary a, Generic a, Typeable a, RecursivelyShrink (Rep a), SubtermsOrigFixed (Rep a)) => a -> [a]
genericShrinkOrigFixed x = subtermsOrigFixed x ++ recursivelyShrink x

-- | All immediate subterms of a term.
subtermsOrigFixed :: (Arbitrary a, Generic a, Typeable a, SubtermsOrigFixed (Rep a)) => a -> [a]
subtermsOrigFixed = gsubtermsOrigFixed . from

class SubtermsOrigFixed f where
  gsubtermsOrigFixed :: (Arbitrary b, Typeable b) => f a -> [b]

instance (SubtermsOrigFixed f, SubtermsOrigFixed g) => SubtermsOrigFixed (f :*: g) where
  gsubtermsOrigFixed (x :*: y) =
    gsubtermsOrigFixed x ++ gsubtermsOrigFixed y

instance (SubtermsOrigFixed f, SubtermsOrigFixed g) => SubtermsOrigFixed (f :+: g) where
  gsubtermsOrigFixed (L1 x) = gsubtermsOrigFixed x
  gsubtermsOrigFixed (R1 x) = gsubtermsOrigFixed x

instance SubtermsOrigFixed f => SubtermsOrigFixed (M1 i c f) where
  gsubtermsOrigFixed (M1 x) = gsubtermsOrigFixed x

instance Typeable a => SubtermsOrigFixed (K1 i a) where
  gsubtermsOrigFixed (K1 x) =
    case cast x of
      Nothing -> []
      Just y -> shrink y

instance SubtermsOrigFixed U1 where
  gsubtermsOrigFixed U1 = []



-- The following is equivalent to subtermsOrig, just that it doesn't need Typeable.

genericShrinkBitonic :: (Generic a, GShrinkBitonic (Rep a) a) => a -> [a]
genericShrinkBitonic = gShrinkBitonic . from

class GShrinkBitonic f b where
  gShrinkBitonic :: f a -> [b]

instance GShrinkBitonic U1 a where
  gShrinkBitonic U1 = []

instance (GShrinkBitonic f b, GShrinkBitonic g b) => GShrinkBitonic (f :*: g) b where
  gShrinkBitonic (l :*: r) = gShrinkBitonic l ++ gShrinkBitonic r

instance (GShrinkBitonic f b, GShrinkBitonic g b) => GShrinkBitonic (f :+: g) b where
  gShrinkBitonic (L1 x) = gShrinkBitonic x
  gShrinkBitonic (R1 x) = gShrinkBitonic x

instance GShrinkBitonic f b => GShrinkBitonic (M1 i c f) b where
  gShrinkBitonic (M1 x) = gShrinkBitonic x

instance GShrinkBitonic (K1 i a) a where
  gShrinkBitonic (K1 x) = [x] -- Bug: this will shrink 0 to [0]

instance GShrinkBitonic (K1 i a) b where
  gShrinkBitonic (K1 _) = []



-- This is equivalent to subtermsOrigFixed, just that it doesn't need Typeable.

genericShrinkBitonicFixed :: (Generic a, GShrinkBitonicFixed (Rep a) a) => a -> [a]
genericShrinkBitonicFixed = gShrinkBitonicFixed . from

class GShrinkBitonicFixed f b where
  gShrinkBitonicFixed :: f a -> [b]

instance GShrinkBitonicFixed U1 a where
  gShrinkBitonicFixed U1 = []

instance (GShrinkBitonicFixed f b, GShrinkBitonicFixed g b) => GShrinkBitonicFixed (f :*: g) b where
  gShrinkBitonicFixed (l :*: r) = gShrinkBitonicFixed l ++ gShrinkBitonicFixed r

instance (GShrinkBitonicFixed f b, GShrinkBitonicFixed g b) => GShrinkBitonicFixed (f :+: g) b where
  gShrinkBitonicFixed (L1 x) = gShrinkBitonicFixed x
  gShrinkBitonicFixed (R1 x) = gShrinkBitonicFixed x

instance GShrinkBitonicFixed f b => GShrinkBitonicFixed (M1 i c f) b where
  gShrinkBitonicFixed (M1 x) = gShrinkBitonicFixed x

instance Arbitrary a => GShrinkBitonicFixed (K1 i a) a where
  gShrinkBitonicFixed (K1 x) = shrink x

instance GShrinkBitonicFixed (K1 i a) b where
  gShrinkBitonicFixed (K1 _) = []



-- The following is correct.

-- | All immediate subterms of a term.
subterms :: (Generic a, Arbitrary a, GSubterms (Rep a) a) => a -> [a]
subterms = gSubterms . from


class GSubterms f a where -- TODO see if I can change this to GSubterms f
  gSubterms :: f a -> [a]

instance GSubterms U1 a where
  gSubterms U1 = []

instance (GSubtermsIncl f a, GSubtermsIncl g a) => GSubterms (f :*: g) a where
  gSubterms (l :*: r) = gSubtermsIncl l ++ gSubtermsIncl r

instance (GSubtermsIncl f a, GSubtermsIncl g a) => GSubterms (f :+: g) a where
  gSubterms (L1 x) = gSubtermsIncl x
  gSubterms (R1 x) = gSubtermsIncl x

instance GSubterms f a => GSubterms (M1 i c f) a where
  gSubterms (M1 x) = gSubterms x

instance GSubterms (K1 i a) b where
  gSubterms (K1 _) = []


-- | subtermsIncl is just for debugging.
subtermsIncl :: (Generic a, Arbitrary a, GSubtermsIncl (Rep a) a) => a -> [a]
subtermsIncl = gSubtermsIncl . from

class GSubtermsIncl f a where
  gSubtermsIncl :: f a -> [a]

instance GSubtermsIncl U1 a where
  gSubtermsIncl U1 = []

instance (GSubtermsIncl f a, GSubtermsIncl g a) => GSubtermsIncl (f :*: g) a where
  gSubtermsIncl (l :*: r) = gSubtermsIncl l ++ gSubtermsIncl r

instance (GSubtermsIncl f a, GSubtermsIncl g a) => GSubtermsIncl (f :+: g) a where
  gSubtermsIncl (L1 x) = gSubtermsIncl x
  gSubtermsIncl (R1 x) = gSubtermsIncl x

instance GSubtermsIncl f a => GSubtermsIncl (M1 i c f) a where
  gSubtermsIncl (M1 x) = gSubtermsIncl x

instance Arbitrary a => GSubtermsIncl (K1 i a) a where
  gSubtermsIncl (K1 x) = [x]

instance GSubtermsIncl (K1 i a) b where
  gSubtermsIncl (K1 _) = []

#endif

-- instances

instance (CoArbitrary a, Arbitrary b) => Arbitrary (a -> b) where
  arbitrary = promote (`coarbitrary` arbitrary)

instance Arbitrary () where
  arbitrary = return ()

instance Arbitrary Bool where
  arbitrary = choose (False,True)
  shrink True = [False]
  shrink False = []

instance Arbitrary Ordering where
  arbitrary = elements [LT, EQ, GT]
  shrink GT = [EQ, LT]
  shrink LT = [EQ]
  shrink EQ = []

instance Arbitrary a => Arbitrary (Maybe a) where
  arbitrary = frequency [(1, return Nothing), (3, liftM Just arbitrary)]

  shrink (Just x) = Nothing : [ Just x' | x' <- shrink x ]
  shrink _        = []

instance (Arbitrary a, Arbitrary b) => Arbitrary (Either a b) where
  arbitrary = oneof [liftM Left arbitrary, liftM Right arbitrary]

  shrink (Left x)  = [ Left  x' | x' <- shrink x ]
  shrink (Right y) = [ Right y' | y' <- shrink y ]

instance Arbitrary a => Arbitrary [a] where
  arbitrary = sized $ \n ->
    do k <- choose (0,n)
       sequence [ arbitrary | _ <- [1..k] ]

  shrink xs = shrinkList shrink xs

-- | Shrink a list of values given a shrinking function for individual values.
shrinkList :: (a -> [a]) -> [a] -> [[a]]
shrinkList shr xs = concat [ removes k n xs | k <- takeWhile (>0) (iterate (`div`2) n) ]
                 ++ shrinkOne xs
 where
  n = length xs

  shrinkOne []     = []
  shrinkOne (x:xs) = [ x':xs | x'  <- shr x ]
                  ++ [ x:xs' | xs' <- shrinkOne xs ]

  removes k n xs
    | k > n     = []
    | null xs2  = [[]]
    | otherwise = xs2 : map (xs1 ++) (removes k (n-k) xs2)
   where
    xs1 = take k xs
    xs2 = drop k xs

{-
  -- "standard" definition for lists:
  shrink []     = []
  shrink (x:xs) = [ xs ]
               ++ [ x:xs' | xs' <- shrink xs ]
               ++ [ x':xs | x'  <- shrink x ]
-}

instance (Integral a, Arbitrary a) => Arbitrary (Ratio a) where
  arbitrary = arbitrarySizedFractional
  shrink    = shrinkRealFracToInteger

instance (RealFloat a, Arbitrary a) => Arbitrary (Complex a) where
  arbitrary = liftM2 (:+) arbitrary arbitrary
  shrink (x :+ y) = [ x' :+ y | x' <- shrink x ] ++
                    [ x :+ y' | y' <- shrink y ]

#ifndef NO_FIXED
instance HasResolution a => Arbitrary (Fixed a) where
  arbitrary = arbitrarySizedFractional
  shrink    = shrinkRealFrac
#endif

instance (Arbitrary a, Arbitrary b)
      => Arbitrary (a,b)
 where
  arbitrary = liftM2 (,) arbitrary arbitrary

  shrink (x, y) =
       [ (x', y) | x' <- shrink x ]
    ++ [ (x, y') | y' <- shrink y ]

instance (Arbitrary a, Arbitrary b, Arbitrary c)
      => Arbitrary (a,b,c)
 where
  arbitrary = liftM3 (,,) arbitrary arbitrary arbitrary

  shrink (x, y, z) =
    [ (x', y', z')
    | (x', (y', z')) <- shrink (x, (y, z)) ]

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d)
      => Arbitrary (a,b,c,d)
 where
  arbitrary = liftM4 (,,,) arbitrary arbitrary arbitrary arbitrary

  shrink (w, x, y, z) =
    [ (w', x', y', z')
    | (w', (x', (y', z'))) <- shrink (w, (x, (y, z))) ]

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d, Arbitrary e)
      => Arbitrary (a,b,c,d,e)
 where
  arbitrary = liftM5 (,,,,) arbitrary arbitrary arbitrary arbitrary arbitrary

  shrink (v, w, x, y, z) =
    [ (v', w', x', y', z')
    | (v', (w', (x', (y', z')))) <- shrink (v, (w, (x, (y, z)))) ]

-- typical instance for primitive (numerical) types

instance Arbitrary Integer where
  arbitrary = arbitrarySizedIntegral
  shrink    = shrinkIntegral

instance Arbitrary Int where
  arbitrary = arbitrarySizedIntegral
  shrink    = shrinkIntegral

instance Arbitrary Int8 where
  arbitrary = arbitrarySizedBoundedIntegral
  shrink    = shrinkIntegral

instance Arbitrary Int16 where
  arbitrary = arbitrarySizedBoundedIntegral
  shrink    = shrinkIntegral

instance Arbitrary Int32 where
  arbitrary = arbitrarySizedBoundedIntegral
  shrink    = shrinkIntegral

instance Arbitrary Int64 where
  arbitrary = arbitrarySizedBoundedIntegral
  shrink    = shrinkIntegral

instance Arbitrary Word where
  arbitrary = arbitrarySizedBoundedIntegral
  shrink    = shrinkIntegral

instance Arbitrary Word8 where
  arbitrary = arbitrarySizedBoundedIntegral
  shrink    = shrinkIntegral

instance Arbitrary Word16 where
  arbitrary = arbitrarySizedBoundedIntegral
  shrink    = shrinkIntegral

instance Arbitrary Word32 where
  arbitrary = arbitrarySizedBoundedIntegral
  shrink    = shrinkIntegral

instance Arbitrary Word64 where
  arbitrary = arbitrarySizedBoundedIntegral
  shrink    = shrinkIntegral

instance Arbitrary Char where
  arbitrary = chr `fmap` oneof [choose (0,127), choose (0,255)]
  shrink c  = filter (<. c) $ nub
            $ ['a','b','c']
           ++ [ toLower c | isUpper c ]
           ++ ['A','B','C']
           ++ ['1','2','3']
           ++ [' ','\n']
   where
    a <. b  = stamp a < stamp b
    stamp a = ( (not (isLower a)
              , not (isUpper a)
              , not (isDigit a))
              , (not (a==' ')
              , not (isSpace a)
              , a)
              )

instance Arbitrary Float where
  arbitrary = arbitrarySizedFractional
  shrink    = shrinkRealFrac

instance Arbitrary Double where
  arbitrary = arbitrarySizedFractional
  shrink    = shrinkRealFrac

-- ** Helper functions for implementing arbitrary

-- | Generates an integral number. The number can be positive or negative
-- and its maximum absolute value depends on the size parameter.
arbitrarySizedIntegral :: Integral a => Gen a
arbitrarySizedIntegral =
  sized $ \n ->
  inBounds fromInteger (choose (-toInteger n, toInteger n))

inBounds :: Integral a => (Integer -> a) -> Gen Integer -> Gen a
inBounds fi g = fmap fi (g `suchThat` (\x -> toInteger (fi x) == x))

-- | Generates a fractional number. The number can be positive or negative
-- and its maximum absolute value depends on the size parameter.
arbitrarySizedFractional :: Fractional a => Gen a
arbitrarySizedFractional =
  sized $ \n ->
    let n' = toInteger n in
      do a <- choose ((-n') * precision, n' * precision)
         b <- choose (1, precision)
         return (fromRational (a % b))
 where
  precision = 9999999999999 :: Integer

-- Useful for getting at minBound and maxBound without having to
-- fiddle around with asTypeOf.
withBounds :: Bounded a => (a -> a -> Gen a) -> Gen a
withBounds k = k minBound maxBound

-- | Generates an integral number. The number is chosen uniformly from
-- the entire range of the type. You may want to use
-- 'arbitrarySizedBoundedIntegral' instead.
arbitraryBoundedIntegral :: (Bounded a, Integral a) => Gen a
arbitraryBoundedIntegral =
  withBounds $ \mn mx ->
  do n <- choose (toInteger mn, toInteger mx)
     return (fromInteger n)

-- | Generates an element of a bounded type. The element is
-- chosen from the entire range of the type.
arbitraryBoundedRandom :: (Bounded a, Random a) => Gen a
arbitraryBoundedRandom = choose (minBound,maxBound)

-- | Generates an element of a bounded enumeration.
arbitraryBoundedEnum :: (Bounded a, Enum a) => Gen a
arbitraryBoundedEnum =
  withBounds $ \mn mx ->
  do n <- choose (fromEnum mn, fromEnum mx)
     return (toEnum n)

-- | Generates an integral number from a bounded domain. The number is
-- chosen from the entire range of the type, but small numbers are
-- generated more often than big numbers. Inspired by demands from
-- Phil Wadler.
arbitrarySizedBoundedIntegral :: (Bounded a, Integral a) => Gen a
arbitrarySizedBoundedIntegral =
  withBounds $ \mn mx ->
  sized $ \s ->
    do let bits n | n `quot` 2 == 0 = 0
                  | otherwise = 1 + bits (n `quot` 2)
           k  = 2^(s*(bits mn `max` bits mx `max` 40) `div` 100)
       n <- choose (toInteger mn `max` (-k), toInteger mx `min` k)
       return (fromInteger n)

-- ** Helper functions for implementing shrink

-- | Returns no shrinking alternatives.
shrinkNothing :: a -> [a]
shrinkNothing _ = []

-- | Shrink an integral number.
shrinkIntegral :: Integral a => a -> [a]
shrinkIntegral x =
  nub $
  [ -x
  | x < 0, -x > x
  ] ++
  [ x'
  | x' <- takeWhile (<< x) (0:[ x - i | i <- tail (iterate (`quot` 2) x) ])
  ]
 where
   -- a << b is "morally" abs a < abs b, but taking care of overflow.
   a << b = case (a >= 0, b >= 0) of
            (True,  True)  -> a < b
            (False, False) -> a > b
            (True,  False) -> a + b < 0
            (False, True)  -> a + b > 0

-- | Shrink a fraction, but only shrink to integral values.
shrinkRealFracToInteger :: RealFrac a => a -> [a]
shrinkRealFracToInteger x =
  nub $
  [ -x
  | x < 0
  ] ++
  map fromInteger (shrinkIntegral (truncate x))

-- | Shrink a fraction.
shrinkRealFrac :: RealFrac a => a -> [a]
shrinkRealFrac x =
  nub $
  shrinkRealFracToInteger x ++
  [ x - x'
  | x' <- take 20 (iterate (/ 2) x)
  , (x - x') << x ]
 where
  a << b = abs a < abs b

--------------------------------------------------------------------------
-- ** CoArbitrary

-- | Used for random generation of functions.
class CoArbitrary a where
  -- | Used to generate a function of type @a -> b@.
  -- The first argument is a value, the second a generator.
  -- You should use 'variant' to perturb the random generator;
  -- the goal is that different values for the first argument will
  -- lead to different calls to 'variant'. An example will help:
  --
  -- @
  -- instance CoArbitrary a => CoArbitrary [a] where
  --   coarbitrary []     = 'variant' 0
  --   coarbitrary (x:xs) = 'variant' 1 . coarbitrary (x,xs)
  -- @

  coarbitrary :: a -> Gen b -> Gen b

{-# DEPRECATED (><) "Use ordinary function composition instead" #-}
-- | Combine two generator perturbing functions, for example the
-- results of calls to 'variant' or 'coarbitrary'.
(><) :: (Gen a -> Gen a) -> (Gen a -> Gen a) -> (Gen a -> Gen a)
(><) = (.)

instance (Arbitrary a, CoArbitrary b) => CoArbitrary (a -> b) where
  coarbitrary f gen =
    do xs <- arbitrary
       coarbitrary (map f xs) gen

instance CoArbitrary () where
  coarbitrary _ = id

instance CoArbitrary Bool where
  coarbitrary False = variant 0
  coarbitrary True  = variant 1

instance CoArbitrary Ordering where
  coarbitrary GT = variant 0
  coarbitrary EQ = variant 1
  coarbitrary LT = variant 2

instance CoArbitrary a => CoArbitrary (Maybe a) where
  coarbitrary Nothing  = variant 0
  coarbitrary (Just x) = variant 1 . coarbitrary x

instance (CoArbitrary a, CoArbitrary b) => CoArbitrary (Either a b) where
  coarbitrary (Left x)  = variant 0 . coarbitrary x
  coarbitrary (Right y) = variant 1 . coarbitrary y

instance CoArbitrary a => CoArbitrary [a] where
  coarbitrary []     = variant 0
  coarbitrary (x:xs) = variant 1 . coarbitrary (x,xs)

instance (Integral a, CoArbitrary a) => CoArbitrary (Ratio a) where
  coarbitrary r = coarbitrary (numerator r,denominator r)

#ifndef NO_FIXED
instance HasResolution a => CoArbitrary (Fixed a) where
  coarbitrary = coarbitraryReal
#endif

instance (RealFloat a, CoArbitrary a) => CoArbitrary (Complex a) where
  coarbitrary (x :+ y) = coarbitrary x . coarbitrary y

instance (CoArbitrary a, CoArbitrary b)
      => CoArbitrary (a,b)
 where
  coarbitrary (x,y) = coarbitrary x
                    . coarbitrary y

instance (CoArbitrary a, CoArbitrary b, CoArbitrary c)
      => CoArbitrary (a,b,c)
 where
  coarbitrary (x,y,z) = coarbitrary x
                      . coarbitrary y
                      . coarbitrary z

instance (CoArbitrary a, CoArbitrary b, CoArbitrary c, CoArbitrary d)
      => CoArbitrary (a,b,c,d)
 where
  coarbitrary (x,y,z,v) = coarbitrary x
                        . coarbitrary y
                        . coarbitrary z
                        . coarbitrary v

instance (CoArbitrary a, CoArbitrary b, CoArbitrary c, CoArbitrary d, CoArbitrary e)
      => CoArbitrary (a,b,c,d,e)
 where
  coarbitrary (x,y,z,v,w) = coarbitrary x
                          . coarbitrary y
                          . coarbitrary z
                          . coarbitrary v
                          . coarbitrary w

-- typical instance for primitive (numerical) types

instance CoArbitrary Integer where
  coarbitrary = coarbitraryIntegral

instance CoArbitrary Int where
  coarbitrary = coarbitraryIntegral

instance CoArbitrary Int8 where
  coarbitrary = coarbitraryIntegral

instance CoArbitrary Int16 where
  coarbitrary = coarbitraryIntegral

instance CoArbitrary Int32 where
  coarbitrary = coarbitraryIntegral

instance CoArbitrary Int64 where
  coarbitrary = coarbitraryIntegral

instance CoArbitrary Word where
  coarbitrary = coarbitraryIntegral

instance CoArbitrary Word8 where
  coarbitrary = coarbitraryIntegral

instance CoArbitrary Word16 where
  coarbitrary = coarbitraryIntegral

instance CoArbitrary Word32 where
  coarbitrary = coarbitraryIntegral

instance CoArbitrary Word64 where
  coarbitrary = coarbitraryIntegral

instance CoArbitrary Char where
  coarbitrary = coarbitrary . ord

instance CoArbitrary Float where
  coarbitrary = coarbitraryReal

instance CoArbitrary Double where
  coarbitrary = coarbitraryReal

-- ** Helpers for implementing coarbitrary

-- | A 'coarbitrary' implementation for integral numbers.
coarbitraryIntegral :: Integral a => a -> Gen b -> Gen b
coarbitraryIntegral = variant

-- | A 'coarbitrary' implementation for real numbers.
coarbitraryReal :: Real a => a -> Gen b -> Gen b
coarbitraryReal x = coarbitrary (toRational x)

-- | 'coarbitrary' helper for lazy people :-).
coarbitraryShow :: Show a => a -> Gen b -> Gen b
coarbitraryShow x = coarbitrary (show x)

-- | A 'coarbitrary' implementation for enums.
coarbitraryEnum :: Enum a => a -> Gen b -> Gen b
coarbitraryEnum = variant . fromEnum

--------------------------------------------------------------------------
-- ** arbitrary generators

-- these are here and not in Gen because of the Arbitrary class constraint

-- | Generates a list of a given length.
vector :: Arbitrary a => Int -> Gen [a]
vector k = vectorOf k arbitrary

-- | Generates an ordered list of a given length.
orderedList :: (Ord a, Arbitrary a) => Gen [a]
orderedList = sort `fmap` arbitrary

-- | Generate an infinite list.
infiniteList :: Arbitrary a => Gen [a]
infiniteList = infiniteListOf arbitrary

--------------------------------------------------------------------------
-- the end.
