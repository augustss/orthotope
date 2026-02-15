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

{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.Array.Internal.Dynamic(
  Array(..), Vector, ShapeL,
  size, shapeL, rank,
  toList, fromList, toVector, fromVector,
  normalize,
  scalar, unScalar, constant,
  reshape, stretch, stretchOuter, transpose,
  index, pad,
  mapA, zipWithA, zipWith3A, zipWith4A, zipWith5A,
  append, concatOuter,
  ravel, unravel,
  window, stride, rotate,
  slice, rerank, rerank2, rev,
  reduce, foldrA, traverseA,
  allSameA,
  sumA, productA, maximumA, minimumA,
  anyA, allA,
  broadcast,
  update,
  generate, iterateN, iota,
  ) where
import Control.DeepSeq
import Data.Coerce(coerce)
import Data.Data(Data)
import GHC.Generics(Generic)
import qualified Data.Vector as V
import GHC.Stack(HasCallStack)
import Test.QuickCheck hiding (generate)
import Text.PrettyPrint.HughesPJClass hiding ((<>))

import qualified Data.Array.DynamicG as G
import Data.Array.Internal(ShapeL, Vector(..), None)

instance Vector V.Vector where
  type VecElem V.Vector = None
  {-# INLINE vIndex #-}
  vIndex = (V.!)
  {-# INLINE vLength #-}
  vLength = V.length
  {-# INLINE vToList #-}
  vToList = V.toList
  {-# INLINE vFromList #-}
  vFromList = V.fromList
  {-# INLINE vFromListN #-}
  vFromListN = V.fromListN
  {-# INLINE vSingleton #-}
  vSingleton = V.singleton
  {-# INLINE vReplicate #-}
  vReplicate = V.replicate
  {-# INLINE vMap #-}
  vMap = fmap
  {-# INLINE vZipWith #-}
  vZipWith = V.zipWith
  {-# INLINE vZipWith3 #-}
  vZipWith3 = V.zipWith3
  {-# INLINE vZipWith4 #-}
  vZipWith4 = V.zipWith4
  {-# INLINE vZipWith5 #-}
  vZipWith5 = V.zipWith5
  {-# INLINE vAppend #-}
  vAppend = (V.++)
  {-# INLINE vConcat #-}
  vConcat = V.concat
  {-# INLINE vFold #-}
  vFold = V.foldl'
  {-# INLINE vSlice #-}
  vSlice = V.slice
  {-# INLINE vSum #-}
  vSum = V.sum
  {-# INLINE vProduct #-}
  vProduct = V.product
  {-# INLINE vMaximum #-}
  vMaximum = V.maximum
  {-# INLINE vMinimum #-}
  vMinimum = V.minimum
  {-# INLINE vUpdate #-}
  vUpdate = (V.//)
  {-# INLINE vGenerate #-}
  vGenerate = V.generate
  {-# INLINE vAll #-}
  vAll = V.all
  {-# INLINE vAny #-}
  vAny = V.any

type role Array nominal
newtype Array a = A { unA :: G.Array V.Vector a }
  deriving (Pretty, Generic, Data)

instance NFData a => NFData (Array a)

instance (Show a) => Show (Array a) where
  showsPrec p = showsPrec p . unA

instance (Read a) => Read (Array a) where
  readsPrec p s = [(A a, r) | (a, r) <- readsPrec p s]

instance Eq (G.Array V.Vector a) => Eq (Array a) where
  x == y = shapeL x == shapeL y && unA x == unA y
  {-# INLINE (==) #-}

instance Ord (G.Array V.Vector a) => Ord (Array a) where
  compare x y = compare (shapeL x) (shapeL y) <> compare (unA x) (unA y)
  {-# INLINE compare #-}

-- | The number of elements in the array.
{-# INLINE size #-}
size :: Array a -> Int
size = product . shapeL

-- | The shape of an array, i.e., a list of the sizes of its dimensions.
-- In the linearization of the array the outermost (i.e. first list element)
-- varies most slowly.
-- O(1) time.
shapeL :: Array a -> ShapeL
shapeL = G.shapeL . unA

-- | The rank of an array, i.e., the number of dimensions it has.
-- O(1) time.
rank :: Array a -> Int
rank = G.rank . unA

-- | Index into an array.  Fails if the array has rank 0 or if the index is out of bounds.
-- O(1) time.
index :: (HasCallStack) => Array a -> Int -> Array a
index a = A . G.index (unA a)

-- | Convert to a list with the elements in the linearization order.
-- O(n) time.
toList :: Array a -> [a]
toList = G.toList . unA

-- | Convert from a list with the elements given in the linearization order.
-- Fails if the given shape does not have the same number of elements as the list.
-- O(n) time.
fromList :: (HasCallStack) => ShapeL -> [a] -> Array a
fromList ss = A . G.fromList ss

-- | Convert to a vector with the elements in the linearization order.
-- O(n) or O(1) time (the latter if the vector is already in the linearization order).
toVector :: (HasCallStack) => Array a -> V.Vector a
toVector = G.toVector . unA

-- | Convert from a vector with the elements given in the linearization order.
-- Fails if the given shape does not have the same number of elements as the list.
-- O(1) time.
fromVector :: (HasCallStack) => ShapeL -> V.Vector a -> Array a
fromVector ss = A . G.fromVector ss

-- | Make sure the underlying vector is in the linearization order.
-- This is semantically an identity function, but can have big performance
-- implications.
-- O(n) or O(1) time.
normalize :: Array a -> Array a
normalize = A . G.normalize . unA

-- | Change the shape of an array.  Fails if the arrays have different number of elements.
-- O(n) or O(1) time.
reshape :: (HasCallStack) => ShapeL -> Array a -> Array a
reshape s = A . G.reshape s . unA

-- | Change the size of dimensions with size 1.  These dimension can be changed to any size.
-- All other dimensions must remain the same.
-- O(1) time.
stretch :: (HasCallStack) => ShapeL -> Array a -> Array a
stretch s = A . G.stretch s . unA

-- | Change the size of the outermost dimension by replication.
stretchOuter :: (HasCallStack) => Int -> Array a -> Array a
stretchOuter s = A . G.stretchOuter s . unA

-- | Convert a value to a scalar (rank 0) array.
-- O(1) time.
scalar :: a -> Array a
scalar = A . G.scalar

-- | Convert a scalar (rank 0) array to a value.
-- O(1) time.
unScalar :: (HasCallStack) => Array a -> a
unScalar = G.unScalar . unA

-- | Make an array with all elements having the same value.
-- O(1) time
constant :: ShapeL -> a -> Array a
constant sh = A . G.constant sh

-- | Map over the array elements.
-- O(n) time.
mapA :: (a -> b) -> Array a -> Array b
mapA f = A . G.mapA f . unA

instance Functor Array where
  fmap = mapA

instance Foldable Array where
  foldr = foldrA

instance Traversable Array where
  traverse = traverseA

-- | Map over the array elements.
-- O(n) time.
zipWithA :: (HasCallStack) => (a -> b -> c) -> Array a -> Array b -> Array c
zipWithA f a b = A $ G.zipWithA f (unA a) (unA b)

-- | Map over the array elements.
-- O(n) time.
zipWith3A :: (HasCallStack) => (a -> b -> c -> d) -> Array a -> Array b -> Array c -> Array d
zipWith3A f a b c = A $ G.zipWith3A f (unA a) (unA b) (unA c)

-- | Map over the array elements.
-- O(n) time.
zipWith4A :: (HasCallStack) => (a -> b -> c -> d -> e) -> Array a -> Array b -> Array c -> Array d -> Array e
zipWith4A f a b c d = A $ G.zipWith4A f (unA a) (unA b) (unA c) (unA d)

-- | Map over the array elements.
-- O(n) time.
zipWith5A :: (HasCallStack) => (a -> b -> c -> d -> e -> f) -> Array a -> Array b -> Array c -> Array d -> Array e -> Array f
zipWith5A f a b c d e = A $ G.zipWith5A f (unA a) (unA b) (unA c) (unA d) (unA e)

-- | Pad each dimension on the low and high side with the given value.
-- O(n) time.
pad :: (HasCallStack) => [(Int, Int)] -> a -> Array a -> Array a
pad ps v = A . G.pad ps v . unA

-- | Do an arbitrary array transposition.
-- Fails if the transposition argument is not a permutation of the numbers
-- [0..r-1], where r is the rank of the array.
-- O(1) time.
transpose :: (HasCallStack) => [Int] -> Array a -> Array a
transpose is = A . G.transpose is . unA

-- | Append two arrays along the outermost dimension.
-- All dimensions, except the outermost, must be the same.
-- O(n) time.
append :: (HasCallStack) => Array a -> Array a -> Array a
append x y = A $ G.append (unA x) (unA y)

-- | Concatenate a number of arrays into a single array.
-- Fails if any, but the outer, dimensions differ.
-- O(n) time.
concatOuter :: (HasCallStack) => [Array a] -> Array a
concatOuter = A . G.concatOuter . coerce

-- | Turn a rank-1 array of arrays into a single array by making the outer array into the outermost
-- dimension of the result array.  All the arrays must have the same shape.
-- O(n) time.
ravel :: (HasCallStack) => Array (Array a) -> Array a
ravel = A . G.ravel . G.mapA unA . unA

-- | Turn an array into a nested array, this is the inverse of 'ravel'.
-- I.e., @ravel . unravel == id@.
unravel :: (HasCallStack) => Array a -> Array (Array a)
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
window :: (HasCallStack) => [Int] -> Array a -> Array a
window ws = A . G.window ws . unA

-- | Stride the outermost dimensions.
-- E.g., if the array shape is @[10,12,8]@ and the strides are
-- @[2,2]@ then the resulting shape will be @[5,6,8]@.
-- O(1) time.
stride :: (HasCallStack) => [Int] -> Array a -> Array a
stride ws = A . G.stride ws . unA

-- | Rotate the array k times along the d'th dimension.
-- E.g., if the array shape is @[2, 3, 2]@, d is 1, and k is 4,
-- the resulting shape will be @[2, 4, 3, 2]@.
rotate :: (HasCallStack) => Int -> Int -> Array a -> Array a
rotate d k = A . G.rotate d k . unA

-- | Extract a slice of an array.
-- The first argument is a list of (offset, length) pairs.
-- The length of the slicing argument must not exceed the rank of the array.
-- The extracted slice must fall within the array dimensions.
-- E.g. @slice [1,2] (fromList [4] [1,2,3,4]) == [2,3]@.
-- O(1) time.
slice :: (HasCallStack) => [(Int, Int)] -> Array a -> Array a
slice ss = A . G.slice ss . unA

-- | Apply a function to the subarrays /n/ levels down and make
-- the results into an array with the same /n/ outermost dimensions.
-- The /n/ must not exceed the rank of the array.
-- O(n) time.
rerank :: (HasCallStack) => Int -> (Array a -> Array b) -> Array a -> Array b
rerank n f = A . G.rerank n (unA . f . A) . unA

-- | Apply a two-argument function to the subarrays /n/ levels down and make
-- the results into an array with the same /n/ outermost dimensions.
-- The /n/ must not exceed the rank of the array.
-- O(n) time.
rerank2 :: (HasCallStack) => Int -> (Array a -> Array b -> Array c) -> Array a -> Array b -> Array c
rerank2 n f ta tb = A $ G.rerank2 n (\ a b -> unA $ f (A a) (A b)) (unA ta) (unA tb)

-- | Reverse the given dimensions, with the outermost being dimension 0.
-- O(1) time.
rev :: [Int] -> Array a -> Array a
rev rs = A . G.rev rs . unA

-- | Reduce all elements of an array into a rank 0 array.
-- To reduce parts use 'rerank' and 'transpose' together with 'reduce'.
-- O(n) time.
reduce :: (a -> a -> a) -> a -> Array a -> Array a
reduce f z = A . G.reduce f z . unA

-- | Constrained version of 'foldr' for Arrays.
--
-- Note that this 'Array' actually has 'Traversable' anyway.
foldrA :: (a -> b -> b) -> b -> Array a -> b
foldrA f z = G.foldrA f z . unA

-- | Constrained version of 'traverse' for Arrays.
--
-- Note that this 'Array' actually has 'Traversable' anyway.
traverseA :: Applicative f => (a -> f b) -> Array a -> f (Array b)
traverseA f = fmap A . G.traverseA f . unA

-- | Check if all elements of the array are equal.
allSameA :: (Eq a) => Array a -> Bool
allSameA = G.allSameA . unA

instance Arbitrary a => Arbitrary (Array a) where arbitrary = A <$> arbitrary

-- | Sum of all elements.
{-# INLINE sumA #-}
sumA :: (Num a) => Array a -> a
sumA = G.sumA . unA

-- | Product of all elements.
{-# INLINE productA #-}
productA :: (Num a) => Array a -> a
productA = G.productA . unA

-- | Maximum of all elements.
{-# INLINE maximumA #-}
maximumA :: (HasCallStack, Ord a) => Array a -> a
maximumA = G.maximumA . unA

-- | Minimum of all elements.
{-# INLINE minimumA #-}
minimumA :: (HasCallStack, Ord a) => Array a -> a
minimumA = G.minimumA . unA

-- | Test if the predicate holds for any element.
{-# INLINE anyA #-}
anyA :: (a -> Bool) -> Array a -> Bool
anyA p = G.anyA p . unA

-- | Test if the predicate holds for all elements.
{-# INLINE allA #-}
allA :: (a -> Bool) -> Array a -> Bool
allA p = G.allA p . unA

-- | Put the dimensions of the argument into the specified dimensions,
-- and just replicate the data along all other dimensions.
-- The list of dimensions indicies must have the same rank as the argument array
-- and it must be strictly ascending.
broadcast :: (HasCallStack) =>
             [Int] -> ShapeL -> Array a -> Array a
broadcast ds sh = A . G.broadcast ds sh . unA

-- | Update the array at the specified indicies to the associated value.
{-# INLINE update #-}
update :: (HasCallStack) =>
          Array a -> [([Int], a)] -> Array a
update a = A . G.update (unA a)

-- | Generate an array with a function that computes the value for each index.
{-# INLINE generate #-}
generate :: ShapeL -> ([Int] -> a) -> Array a
generate sh = A . G.generate sh

-- | Iterate a function n times.
{-# INLINE iterateN #-}
iterateN :: Int -> (a -> a) -> a -> Array a
iterateN n f = A . G.iterateN n f

-- | Generate a vector from 0 to n-1.
{-# INLINE iota #-}
iota :: (Enum a, Num a) => Int -> Array a
iota = A . G.iota
