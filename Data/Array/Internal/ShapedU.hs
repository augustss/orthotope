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
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
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
module Data.Array.Internal.ShapedU(
  Array(..), Shape(..), Size, Rank, Vector, ShapeL, Unbox,
  Window, Stride, Permute, Permutation, ValidDims,
  size, shapeL, rank,
  toList, fromList, toVector, fromVector,
  normalize,
  scalar, unScalar, constant,
  reshape, stretch, stretchOuter, transpose,
  index, pad,
  mapA, zipWithA, zipWith3A,
  append,
  ravel, unravel,
  window, stride,
  slice, rerank, rerank2, rev,
  reduce, foldrA, traverseA,
  allSameA,
  sumA, productA, minimumA, maximumA,
  allA, anyA,
  broadcast,
  generate, iterateN, iota,
  ) where
import Control.DeepSeq
import Data.Data(Data)
import qualified Data.Vector.Unboxed as V
import GHC.Generics(Generic)
import GHC.TypeLits(KnownNat, type (+), type (<=))
import Test.QuickCheck hiding (generate)
import Text.PrettyPrint.HughesPJClass hiding ((<>))

import Data.Array.Internal.DynamicU()  -- Vector instance
import qualified Data.Array.Internal.ShapedG as G
import Data.Array.Internal(ShapeL, Vector)
import Data.Array.Internal.Shape

type Unbox = V.Unbox

type role Array nominal nominal
newtype Array sh a = A { unA :: G.Array sh V.Vector a }
  deriving (Pretty, Generic, Data)

instance NFData (Array sh a)

instance (Unbox a, Show a, Shape sh) => Show (Array sh a) where
  showsPrec p = showsPrec p . unA

instance (Read a, Unbox a, Shape sh) => Read (Array sh a) where
  readsPrec p s = [(A a, r) | (a, r) <- readsPrec p s]

instance Eq (G.Array sh V.Vector a) => Eq (Array sh a) where
  x == y = unA x == unA y
  {-# INLINE (==) #-}

instance Ord (G.Array sh V.Vector a) => Ord (Array sh a) where
  compare x y = compare (unA x) (unA y)
  {-# INLINE compare #-}

-- | The number of elements in the array.
{-# INLINE size #-}
size :: forall sh a . (Shape sh) => Array sh a -> Int
size = G.size . unA

-- | The shape of an array, i.e., a list of the sizes of its dimensions.
-- In the linearization of the array the outermost (i.e. first list element)
-- varies most slowly.
-- O(1) time.
shapeL :: (Shape sh) => Array sh a -> ShapeL
shapeL = G.shapeL . unA

-- | The rank of an array, i.e., the number of dimensions it has,
-- which is the @n@ in @Array n a@.
-- O(1) time.
rank :: (Shape sh, KnownNat (Rank sh)) => Array sh a -> Int
rank = G.rank . unA

-- | Index into an array.  Fails if the index is out of bounds.
-- O(1) time.
index :: (Unbox a, KnownNat s) => Array (s:sh) a -> Int -> Array sh a
index a = A . G.index (unA a)

-- | Convert to a list with the elements in the linearization order.
-- O(n) time.
toList :: forall sh a . (Unbox a, Shape sh) => Array sh a -> [a]
toList = G.toList . unA

-- | Convert from a list with the elements given in the linearization order.
-- Fails if the given shape does not have the same number of elements as the list.
-- O(n) time.
fromList :: forall sh a . (Unbox a, Shape sh) => [a] -> Array sh a
fromList = A . G.fromList

-- | Convert to a vector with the elements in the linearization order.
-- O(n) or O(1) time (the latter if the vector is already in the linearization order).
toVector :: (Unbox a, Shape sh) => Array sh a -> V.Vector a
toVector = G.toVector . unA

-- | Convert from a vector with the elements given in the linearization order.
-- Fails if the given shape does not have the same number of elements as the list.
-- O(1) time.
fromVector :: forall sh a . (Unbox a, Shape sh) => V.Vector a -> Array sh a
fromVector = A . G.fromVector

-- | Make sure the underlying vector is in the linearization order.
-- This is semantically an identity function, but can have big performance
-- implications.
-- O(n) or O(1) time.
normalize :: (Unbox a, Shape sh) => Array sh a -> Array sh a
normalize = A . G.normalize . unA

-- | Change the shape of an array.  Type error if the arrays have different number of elements.
-- O(n) or O(1) time.
reshape :: forall sh' sh a . (Unbox a, Shape sh, Shape sh', Size sh ~ Size sh') =>
           Array sh a -> Array sh' a
reshape = A . G.reshape . unA

-- | Change the size of dimensions with size 1.  These dimension can be changed to any size.
-- All other dimensions must remain the same.
-- O(1) time.
stretch :: forall sh' sh a . (Shape sh, Shape sh', ValidStretch sh sh') => Array sh a -> Array sh' a
stretch = A . G.stretch . unA

-- | Change the size of the outermost dimension by replication.
stretchOuter :: (KnownNat s, Shape sh) => Array (1 : sh) a -> Array (s : sh) a
stretchOuter = A . G.stretchOuter . unA

-- | Convert a value to a scalar (rank 0) array.
-- O(1) time.
scalar :: (Unbox a) => a -> Array '[] a
scalar = A . G.scalar

-- | Convert a scalar (rank 0) array to a value.
-- O(1) time.
unScalar :: (Unbox a) => Array '[] a -> a
unScalar = G.unScalar . unA

-- | Make an array with all elements having the same value.
-- O(1) time.
constant :: forall sh a . (Unbox a, Shape sh) =>
            a -> Array sh a
constant = A . G.constant

-- | Map over the array elements.
-- O(n) time.
mapA :: (Unbox a, Unbox b, Shape sh) => (a -> b) -> Array sh a -> Array sh b
mapA f = A . G.mapA f . unA

-- | Map over the array elements.
-- O(n) time.
zipWithA :: (Unbox a, Unbox b, Unbox c, Shape sh) => (a -> b -> c) -> Array sh a -> Array sh b -> Array sh c
zipWithA f a b = A $ G.zipWithA f (unA a) (unA b)

-- | Map over the array elements.
-- O(n) time.
zipWith3A :: (Unbox a, Unbox b, Unbox c, Unbox d, Shape sh) => (a -> b -> c -> d) -> Array sh a -> Array sh b -> Array sh c -> Array sh d
zipWith3A f a b c = A $ G.zipWith3A f (unA a) (unA b) (unA c)

-- | Pad each dimension on the low and high side with the given value.
-- O(n) time.
pad :: forall ps sh' sh a . (Unbox a, Padded ps sh sh', Shape sh) =>
       a -> Array sh a -> Array sh' a
pad v = A . G.pad @ps v . unA

-- | Do an arbitrary array transposition.
-- Fails if the transposition argument is not a permutation of the numbers
-- [0..r-1], where r is the rank of the array.
-- O(1) time.
transpose :: forall is sh a .
             (Permutation is, Rank is <= Rank sh, Shape sh, Shape is, KnownNat (Rank sh)) =>
             Array sh a -> Array (Permute is sh) a
transpose = A . G.transpose @is . unA

-- | Append two arrays along the outermost dimension.
-- All dimensions, except the outermost, must be the same.
-- O(n) time.
append :: (Unbox a, Shape sh, KnownNat m, KnownNat n, KnownNat (m+n)) =>
          Array (m ': sh) a -> Array (n ': sh) a -> Array (m+n ': sh) a
append x y = A $ G.append (unA x) (unA y)

-- | Turn a rank-1 array of arrays into a single array by making the outer array into the outermost
-- dimension of the result array.  All the arrays must have the same shape.
-- O(n) time.
ravel :: (Unbox a, Unbox (Array sh a), Unbox (G.Array sh V.Vector a), Shape sh, KnownNat s) =>
         Array '[s] (Array sh a) -> Array (s:sh) a
ravel = A . G.ravel . G.mapA unA . unA

-- | Turn an array into a nested array, this is the inverse of 'ravel'.
-- I.e., @ravel . unravel == id@.
unravel :: (Unbox a, Unbox (Array sh a), Unbox (G.Array sh V.Vector a), Shape sh, KnownNat s) =>
           Array (s:sh) a -> Array '[s] (Array sh a)
unravel = A . G.mapA A . G.unravel . unA

-- | Make a window of the outermost dimensions.
-- The rank increases with the length of the window list.
-- E.g., if the shape of the array is @[10,12,8]@ and
-- the window size is @[3,3]@ then the resulting array will have shape
-- @[8,10,3,3,8]@.
-- O(1) time.
window :: forall ws sh' sh a .
          (Window ws sh sh', KnownNat (Rank ws)) =>
          Array sh a -> Array sh' a
window = A . G.window @ws . unA

-- | Stride the outermost dimensions.
-- E.g., if the array shape is @[10,12,8]@ and the strides are
-- @[2,2]@ then the resulting shape will be @[5,6,8]@.
-- O(1) time.
stride :: forall ts sh' sh a .
          (Stride ts sh sh', Shape ts) =>
          Array sh a -> Array sh' a
stride = A . G.stride @ts . unA

-- | Extract a slice of an array.
-- The first argument is a list of (offset, length) pairs.
-- The length of the slicing argument must not exceed the rank of the array.
-- The extracted slice must fall within the array dimensions.
-- E.g. @slice [1,2] (fromList [4] [1,2,3,4]) == [2,3]@.
-- O(1) time.
slice :: forall sl sh' sh a .
         (Slice sl sh sh') =>
         Array sh a -> Array sh' a
slice = A . G.slice @sl . unA

-- | Apply a function to the subarrays /n/ levels down and make
-- the results into an array with the same /n/ outermost dimensions.
-- The /n/ must not exceed the rank of the array.
-- O(n) time.
rerank :: forall n i o sh a b .
          (Unbox a, Unbox b,
           Drop n sh ~ i, Shape sh, KnownNat n, Shape o, Shape (Take n sh ++ o)) =>
          (Array i a -> Array o b) -> Array sh a -> Array (Take n sh ++ o) b
rerank f = A . G.rerank @n (unA . f . A) . unA

-- | Apply a two-argument function to the subarrays /n/ levels down and make
-- the results into an array with the same /n/ outermost dimensions.
-- The /n/ must not exceed the rank of the array.
-- O(n) time.
rerank2 :: forall n i o sh a b c .
           (Unbox a, Unbox b, Unbox c,
            Drop n sh ~ i, Shape sh, KnownNat n, Shape o, Shape (Take n sh ++ o)) =>
           (Array i a -> Array i b -> Array o c) -> Array sh a -> Array sh b -> Array (Take n sh ++ o) c
rerank2 f ta tb = A $ G.rerank2 @n (\ a b -> unA $ f (A a) (A b)) (unA ta) (unA tb)

-- | Reverse the given dimensions, with the outermost being dimension 0.
-- O(1) time.
rev :: forall rs sh a . (ValidDims rs sh, Shape rs, Shape sh) =>
       Array sh a -> Array sh a
rev = A . G.rev @rs . unA

-- | Reduce all elements of an array into a rank 0 array.
-- To reduce parts use 'rerank' and 'transpose' together with 'reduce'.
-- O(n) time.
reduce :: (Unbox a, Shape sh) => (a -> a -> a) -> a -> Array sh a -> Array '[] a
reduce f z = A . G.reduce f z . unA

-- | Constrained version of 'foldr' for Arrays.
foldrA :: (Unbox a, Shape sh) => (a -> b -> b) -> b -> Array sh a -> b
foldrA f z = G.foldrA f z . unA

-- | Constrained version of 'traverse' for Arrays.
traverseA
  :: (Unbox a, Unbox b, Applicative f, Shape sh)
  => (a -> f b) -> Array sh a -> f (Array sh b)
traverseA f = fmap A . G.traverseA f . unA

-- | Check if all elements of the array are equal.
allSameA :: (Shape sh, Unbox a, Eq a) => Array sh a -> Bool
allSameA = G.allSameA . unA

instance (Shape sh, Arbitrary a, Unbox a) => Arbitrary (Array sh a) where arbitrary = A <$> arbitrary
-- | Sum of all elements.

{-# INLINE sumA #-}
sumA :: (Unbox a, Num a, Shape sh) => Array sh a -> a
sumA = G.sumA . unA

-- | Product of all elements.
{-# INLINE productA #-}
productA :: (Unbox a, Num a, Shape sh) => Array sh a -> a
productA = G.productA . unA

-- | Maximum of all elements.
{-# INLINE maximumA #-}
maximumA :: (Unbox a, Ord a, Shape sh, 1 <= Size sh) => Array sh a -> a
maximumA = G.maximumA . unA

-- | Minimum of all elements.
{-# INLINE minimumA #-}
minimumA :: (Unbox a, Ord a, Shape sh, 1 <= Size sh) => Array sh a -> a
minimumA = G.minimumA . unA

-- | Test if the predicate holds for any element.
{-# INLINE anyA #-}
anyA :: (Shape sh, Unbox a) => (a -> Bool) -> Array sh a -> Bool
anyA p = G.anyA p . unA

-- | Test if the predicate holds for all elements.
{-# INLINE allA #-}
allA :: (Shape sh, Unbox a) => (a -> Bool) -> Array sh a -> Bool
allA p = G.allA p . unA

-- | Put the dimensions of the argument into the specified dimensions,
-- and just replicate the data along all other dimensions.
-- The list of dimensions indicies must have the same rank as the argument array
-- and it must be strictly ascending.
{-# INLINE broadcast #-}
broadcast :: forall ds sh' sh a .
             (Unbox a, Shape sh, Shape sh',
              G.Broadcast ds sh sh') =>
             Array sh a -> Array sh' a
broadcast = A . G.broadcast @ds @sh' @sh . unA

-- | Generate an array with a function that computes the value for each index.
{-# INLINE generate #-}
generate :: (Unbox a, Shape sh) => ([Int] -> a) -> Array sh a
generate = A . G.generate

-- | Iterate a function n times.
{-# INLINE iterateN #-}
iterateN :: forall n a .
            (Unbox a, KnownNat n) => (a -> a) -> a -> Array '[n] a
iterateN f = A . G.iterateN f

-- | Generate a vector from 0 to n-1.
{-# INLINE iota #-}
iota :: (KnownNat n, Unbox a, Enum a, Num a) => Array '[n] a
iota = A G.iota
