-- Copyright 2020 Google LLC
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
--
--      http://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.Array.Internal.RankedU(
  Array(..), Vector, ShapeL, Unbox,
  size, shapeL, rank,
  toList, fromList, toVector, fromVector,
  normalize,
  scalar, unScalar, constant,
  reshape, stretch, stretchOuter, transpose,
  index, pad,
  mapA, zipWithA, zipWith3A,
  append, concatOuter,
  ravel, unravel,
  window, stride, rotate,
  slice, rerank, rerank2, rev,
  reduce, foldrA, traverseA,
  allSameA,
  sumA, productA, minimumA, maximumA,
  anyA, allA,
  broadcast,
  generate, iterateN, iota,
  ) where
import Control.DeepSeq
import Data.Coerce(coerce)
import Data.Data(Data)
import qualified Data.Vector.Unboxed as V
import GHC.TypeLits(KnownNat, type (+), type (<=))
import Test.QuickCheck hiding (generate)
import GHC.Generics(Generic)
import GHC.Stack(HasCallStack)
import Text.PrettyPrint.HughesPJClass hiding ((<>))

import Data.Array.Internal.DynamicU()  -- Vector instance
import qualified Data.Array.Internal.RankedG as G
import Data.Array.Internal(ShapeL, Vector(..))

type Unbox = V.Unbox

type role Array nominal nominal
newtype Array n a = A { unA :: G.Array n V.Vector a }
  deriving (Pretty, Generic, Data)

instance NFData (Array n a)

instance (Show a, Unbox a) => Show (Array n a) where
  showsPrec p = showsPrec p . unA

instance (KnownNat n, Read a, Unbox a) => Read (Array n a) where
  readsPrec p s = [(A a, r) | (a, r) <- readsPrec p s]

instance Eq (G.Array n V.Vector a) => Eq (Array n a) where
  x == y = shapeL x == shapeL y && unA x == unA y
  {-# INLINE (==) #-}

instance Ord (G.Array n V.Vector a) => Ord (Array n a) where
  compare x y = compare (shapeL x) (shapeL y) <> compare (unA x) (unA y)
  {-# INLINE compare #-}

-- | The number of elements in the array.
{-# INLINE size #-}
size :: Array n a -> Int
size = product . shapeL

-- | The shape of an array, i.e., a list of the sizes of its dimensions.
-- In the linearization of the array the outermost (i.e. first list element)
-- varies most slowly.
-- O(1) time.
shapeL :: Array n a -> ShapeL
shapeL = G.shapeL . unA

-- | The rank of an array, i.e., the number of dimensions it has,
-- which is the @n@ in @Array n a@.
-- O(1) time.
rank :: (KnownNat n) => Array n a -> Int
rank = G.rank . unA

-- | Index into an array.  Fails if the index is out of bounds.
-- O(1) time.
index :: (Unbox a) => Array (1+n) a -> Int -> Array n a
index a = A . G.index (unA a)

-- | Convert to a list with the elements in the linearization order.
-- O(n) time.
toList :: (Unbox a) => Array n a -> [a]
toList = G.toList . unA

-- | Convert from a list with the elements given in the linearization order.
-- Fails if the given shape does not have the same number of elements as the list.
-- O(n) time.
fromList :: (Unbox a, KnownNat n) => ShapeL -> [a] -> Array n a
fromList ss = A . G.fromList ss

-- | Convert to a vector with the elements in the linearization order.
-- O(n) or O(1) time (the latter if the vector is already in the linearization order).
toVector :: (Unbox a) => Array n a -> V.Vector a
toVector = G.toVector . unA

-- | Convert from a vector with the elements given in the linearization order.
-- Fails if the given shape does not have the same number of elements as the list.
-- O(1) time.
fromVector :: (Unbox a, KnownNat n) => ShapeL -> V.Vector a -> Array n a
fromVector ss = A . G.fromVector ss

-- | Make sure the underlying vector is in the linearization order.
-- This is semantically an identity function, but can have big performance
-- implications.
-- O(n) or O(1) time.
normalize :: (Unbox a, KnownNat n) => Array n a -> Array n a
normalize = A . G.normalize . unA

-- | Change the shape of an array.  Fails if the arrays have different number of elements.
-- O(n) or O(1) time.
reshape :: (Unbox a, KnownNat n, KnownNat n') => ShapeL -> Array n a -> Array n' a
reshape s = A . G.reshape s . unA

-- | Change the size of dimensions with size 1.  These dimension can be changed to any size.
-- All other dimensions must remain the same.
-- O(1) time.
stretch :: ShapeL -> Array n a -> Array n a
stretch s = A . G.stretch s . unA

-- | Change the size of the outermost dimension by replication.
stretchOuter :: (HasCallStack, 1 <= n) => Int -> Array n a -> Array n a
stretchOuter s = A . G.stretchOuter s . unA

-- | Convert a value to a scalar (rank 0) array.
-- O(1) time.
scalar :: (Unbox a) => a -> Array 0 a
scalar = A . G.scalar

-- | Convert a scalar (rank 0) array to a value.
-- O(1) time.
unScalar :: (Unbox a) => Array 0 a -> a
unScalar = G.unScalar . unA

-- | Make an array with all elements having the same value.
-- O(1) time
constant :: (Unbox a, KnownNat n) => ShapeL -> a -> Array n a
constant sh = A . G.constant sh

-- | Map over the array elements.
-- O(n) time.
mapA :: (Unbox a, Unbox b) =>
        (a -> b) -> Array n a -> Array n b
mapA f = A . G.mapA f . unA

-- | Map over the array elements.
-- O(n) time.
zipWithA :: (Unbox a, Unbox b, Unbox c) =>
            (a -> b -> c) -> Array n a -> Array n b -> Array n c
zipWithA f a b = A $ G.zipWithA f (unA a) (unA b)

-- | Map over the array elements.
-- O(n) time.
zipWith3A :: (Unbox a, Unbox b, Unbox c, Unbox d) =>
             (a -> b -> c -> d) -> Array n a -> Array n b -> Array n c -> Array n d
zipWith3A f a b c = A $ G.zipWith3A f (unA a) (unA b) (unA c)

-- | Pad each dimension on the low and high side with the given value.
-- O(n) time.
pad :: (Unbox a, KnownNat n) => [(Int, Int)] -> a -> Array n a -> Array n a
pad ps v = A . G.pad ps v . unA

-- | Do an arbitrary array transposition.
-- Fails if the transposition argument is not a permutation of the numbers
-- [0..r-1], where r is the rank of the array.
-- O(1) time.
transpose :: (KnownNat n) => [Int] -> Array n a -> Array n a
transpose is = A . G.transpose is . unA

-- | Append two arrays along the outermost dimension.
-- All dimensions, except the outermost, must be the same.
-- O(n) time.
append :: (Unbox a, KnownNat n) => Array n a -> Array n a -> Array n a
append x y = A $ G.append (unA x) (unA y)

-- | Concatenate a number of arrays into a single array.
-- Fails if any, but the outer, dimensions differ.
-- O(n) time.
concatOuter :: (Unbox a, KnownNat n) => [Array n a] -> Array n a
concatOuter = A . G.concatOuter . coerce

-- | Turn a rank-1 array of arrays into a single array by making the outer array into the outermost
-- dimension of the result array.  All the arrays must have the same shape.
-- O(n) time.
ravel :: (Unbox a, Unbox (Array n a), Unbox (G.Array n V.Vector a), KnownNat (1+n)) =>
         Array 1 (Array n a) -> Array (1+n) a
ravel = A . G.ravel . G.mapA unA . unA

-- | Turn an array into a nested array, this is the inverse of 'ravel'.
-- I.e., @ravel . unravel == id@.
unravel :: (Unbox a, Unbox (Array n a), Unbox (G.Array n V.Vector a)) =>
           Array (1+n) a -> Array 1 (Array n a)
unravel = A . G.mapA A . G.unravel . unA

-- | Make a window of the outermost dimensions.
-- The rank increases with the length of the window list.
-- E.g., if the shape of the array is @[10,12,8]@ and
-- the window size is @[3,3]@ then the resulting array will have shape
-- @[8,10,3,3,8]@.
--
-- E.g., @window [2] (fromList [4] [1,2,3,4]) == fromList [3,2] [1,2, 2,3, 3,4]@
-- O(1) time.
--
-- If the window parameter @ws = [w1,...,wk]@ and @wa = window ws a@ then
-- @wa `index` i1 ... `index` ik == slice [(i1,w1),...,(ik,wk)] a@.
window :: (KnownNat n, KnownNat n') => [Int] -> Array n a -> Array n' a
window ws = A . G.window ws . unA

-- | Stride the outermost dimensions.
-- E.g., if the array shape is @[10,12,8]@ and the strides are
-- @[2,2]@ then the resulting shape will be @[5,6,8]@.
-- O(1) time.
stride :: [Int] -> Array n a -> Array n a
stride ws = A . G.stride ws . unA

-- | Rotate the array k times along the d'th dimension.
-- E.g., if the array shape is @[2, 3, 2]@, d is 1, and k is 4,
-- the resulting shape will be @[2, 4, 3, 2]@.
rotate :: forall d p a.
          (KnownNat p, KnownNat d, Unbox a,
          -- Nonsense
          (d + (p + 1)) ~ ((p + d) + 1),
          (d + p) ~ (p + d),
          1 <= p + 1,
          KnownNat ((p + d) + 1),
          KnownNat (p + 1),
          KnownNat (1 + (p + 1))
          ) =>
          Int -> Array (p + d) a -> Array (p + d + 1) a
rotate k = A . G.rotate @d @p k . unA

-- | Extract a slice of an array.
-- The first argument is a list of (offset, length) pairs.
-- The length of the slicing argument must not exceed the rank of the array.
-- The extracted slice must fall within the array dimensions.
-- E.g. @slice [1,2] (fromList [4] [1,2,3,4]) == [2,3]@.
-- O(1) time.
slice :: [(Int, Int)] -> Array n a -> Array n a
slice ss = A . G.slice ss . unA

-- | Apply a function to the subarrays /n/ levels down and make
-- the results into an array with the same /n/ outermost dimensions.
-- The /n/ must not exceed the rank of the array.
-- O(1) time.
rerank :: forall n i o a b .
          (Unbox a, Unbox b, KnownNat n, KnownNat o, KnownNat (n+o), KnownNat (1+o)) =>
          (Array i a -> Array o b) -> Array (n+i) a -> Array (n+o) b
rerank f = A . G.rerank (unA . f . A) . unA

-- | Apply a two-argument function to the subarrays /n/ levels down and make
-- the results into an array with the same /n/ outermost dimensions.
-- The /n/ must not exceed the rank of the array.
-- O(n) time.
rerank2 :: forall n i o a b c .
           (Unbox a, Unbox b, Unbox c, KnownNat n, KnownNat o, KnownNat (n+o), KnownNat (1+o)) =>
           (Array i a -> Array i b -> Array o c) -> Array (n+i) a -> Array (n+i) b -> Array (n+o) c
rerank2 f ta tb = A $ G.rerank2 @n (\ a b -> unA $ f (A a) (A b)) (unA ta) (unA tb)

-- | Reverse the given dimensions, with the outermost being dimension 0.
-- O(1) time.
rev :: [Int] -> Array n a -> Array n a
rev rs = A . G.rev rs . unA

-- | Reduce all elements of an array into a rank 0 array.
-- To reduce parts use 'rerank' and 'transpose' together with 'reduce'.
-- O(n) time.
reduce :: (Unbox a) => (a -> a -> a) -> a -> Array n a -> Array 0 a
reduce f z = A . G.reduce f z . unA

-- | Constrained version of 'foldr' for Arrays.
foldrA :: (Unbox a) => (a -> b -> b) -> b -> Array n a -> b
foldrA f z = G.foldrA f z . unA

-- | Constrained version of 'traverse' for Arrays.
traverseA
  :: (Unbox a, Unbox b, Applicative f)
  => (a -> f b) -> Array n a -> f (Array n b)
traverseA f = fmap A . G.traverseA f . unA

-- | Check if all elements of the array are equal.
allSameA :: (Unbox a, Eq a) => Array n a -> Bool
allSameA = G.allSameA . unA

instance (KnownNat r, Arbitrary a, Unbox a) => Arbitrary (Array r a) where arbitrary = A <$> arbitrary

-- | Sum of all elements.
{-# INLINE sumA #-}
sumA :: (Unbox a, Num a) => Array r a -> a
sumA = G.sumA . unA

-- | Product of all elements.
{-# INLINE productA #-}
productA :: (Unbox a, Num a) => Array r a -> a
productA = G.productA . unA

-- | Maximum of all elements.
{-# INLINE maximumA #-}
maximumA :: (Unbox a, Ord a) => Array r a -> a
maximumA = G.maximumA . unA

-- | Minimum of all elements.
{-# INLINE minimumA #-}
minimumA :: (Unbox a, Ord a) => Array r a -> a
minimumA = G.minimumA . unA

-- | Test if the predicate holds for any element.
{-# INLINE anyA #-}
anyA :: Unbox a => (a -> Bool) -> Array r a -> Bool
anyA p = G.anyA p . unA

-- | Test if the predicate holds for all elements.
{-# INLINE allA #-}
allA :: Unbox a => (a -> Bool) -> Array r a -> Bool
allA p = G.allA p . unA

-- | Put the dimensions of the argument into the specified dimensions,
-- and just replicate the data along all other dimensions.
-- The list of dimensions indicies must have the same rank as the argument array
-- and it must be strictly ascending.
broadcast :: forall r' r a .
             (HasCallStack, Unbox a, KnownNat r, KnownNat r') =>
             [Int] -> ShapeL -> Array r a -> Array r' a
broadcast ds sh = A . G.broadcast ds sh . unA

-- | Generate an array with a function that computes the value for each index.
{-# INLINE generate #-}
generate :: forall n a . (KnownNat n, Unbox a) =>
            ShapeL -> ([Int] -> a) -> Array n a
generate sh = A . G.generate sh

-- | Iterate a function n times.
{-# INLINE iterateN #-}
iterateN :: forall a . (Unbox a) =>
            Int -> (a -> a) -> a -> Array 1 a
iterateN n f = A . G.iterateN n f

-- | Generate a vector from 0 to n-1.
{-# INLINE iota #-}
iota :: (Unbox a, Enum a, Num a) => Int -> Array 1 a
iota = A . G.iota
