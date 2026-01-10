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

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
-- | Arrays of static size.  The arrays are polymorphic in the underlying
-- linear data structure used to store the actual values.
module Data.Array.Internal.ShapedG(
  Array(..), Shape(..), Size, Rank, Vector, VecElem,
  Window, Stride, Permute, Permutation, ValidDims,
  Broadcast,
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
  anyA, allA,
  broadcast,
  generate, iterateN, iota,
  ) where
import Control.DeepSeq
import Data.Data(Data)
import Data.Proxy(Proxy(..))
import GHC.Generics(Generic)
import GHC.Stack(HasCallStack)
import GHC.TypeLits(Nat, type (<=), KnownNat, type (+))
import Test.QuickCheck hiding (generate)
import Text.PrettyPrint.HughesPJClass

import Data.Array.Internal
import Data.Array.Internal.Shape

-- | Arrays stored in a /v/ with values of type /a/.
type role Array nominal representational nominal
newtype Array (sh :: [Nat]) v a = A (T v a)
  deriving (Generic, Data)

instance (Vector v, Show a, VecElem v a, Shape sh, Show (v a)) => Show (Array sh v a) where
  {-# INLINABLE showsPrec #-}
  showsPrec p a@(A _) = showParen (p > 10) $
    showString "fromList @" . showsPrec 11 (shapeL a) . showString" " . showsPrec 11 (toList a)

instance (Shape sh, Vector v, Read a, VecElem v a) => Read (Array sh v a) where
  readsPrec p = readParen (p > 10) $ \ r1 ->
    [(fromList xs, r4)
    | ("fromList", r2) <- lex r1, ("@", r2') <- lex r2
    , (s, r3) <- readsPrec 11 r2', (xs, r4) <- readsPrec 11 r3
    , s == shapeP (Proxy :: Proxy sh), product s == length xs]

instance (Vector v, Eq a, VecElem v a, Eq (v a), Shape sh)
         => Eq (Array sh v a) where
  a@(A v) == (A v') = equalT (shapeL a) v v'
  {-# INLINE (==) #-}

instance (Vector v, Ord a, Ord (v a), VecElem v a, Shape sh)
         => Ord (Array sh v a) where
  a@(A v) `compare` (A v') = compareT (shapeL a) v v'
  {-# INLINE compare #-}

instance (Vector v, Pretty a, VecElem v a, Shape sh) => Pretty (Array sh v a) where
  pPrintPrec l p a@(A t) = ppT l p (shapeL a) t

instance (NFData (v a)) => NFData (Array sh v a) where
  rnf (A t) = rnf t

-- | The number of elements in the array.
{-# INLINE size #-}
size :: forall sh v a . (Shape sh) => Array sh v a -> Int
size _ = sizeP (Proxy :: Proxy sh)

-- | The shape of an array, i.e., a list of the sizes of its dimensions.
-- In the linearization of the array the outermost (i.e. first list element)
-- varies most slowly.
-- O(1) time.
{-# INLINE shapeL #-}
shapeL :: forall sh v a . (Shape sh) => Array sh v a -> ShapeL
shapeL _ = shapeP (Proxy :: Proxy sh)

-- | The rank of an array, i.e., the number of dimensions it has.
-- O(1) time.
{-# INLINE rank #-}
rank :: forall sh v a . (Shape sh, KnownNat (Rank sh)) => Array sh v a -> Int
rank _ = valueOf @(Rank sh)

-- | Index into an array.  Fails if the array has rank 0 or if the index is out of bounds.
-- O(1) time.
{-# INLINE index #-}
index :: forall s sh v a . (HasCallStack, Vector v, KnownNat s) =>
         Array (s:sh) v a -> Int -> Array sh v a
index (A t) i | i < 0 || i >= s = error $ "index: out of bounds: " ++ show i ++ " >= " ++ show s
              | otherwise = A $ indexT t i
  where s = valueOf @s

-- | Convert to a list with the elements in the linearization order.
-- O(n) time.
{-# INLINE toList #-}
toList :: (Vector v, VecElem v a, Shape sh) => Array sh v a -> [a]
toList a@(A t) = toListT (shapeL a) t

-- | Convert to a vector with the elements in the linearization order.
-- O(n) or O(1) time (the latter if the vector is already in the linearization order).
{-# INLINE toVector #-}
toVector :: (Vector v, VecElem v a, Shape sh) => Array sh v a -> v a
toVector a@(A t) = toVectorT (shapeL a) t

-- | Convert from a list with the elements given in the linearization order.
-- Fails if the given shape does not have the same number of elements as the list.
-- O(n) time.
{-# INLINE fromList #-}
fromList :: forall sh v a . (HasCallStack, Vector v, VecElem v a, Shape sh) =>
            [a] -> Array sh v a
fromList vs | n /= l = error $ "fromList: size mismatch " ++ show (n, l)
            | otherwise = A $ T st 0 $ vFromListN l vs
  where n : st = getStridesT ss
        l = length vs
        ss = shapeP (Proxy :: Proxy sh)

-- | Convert from a vector with the elements given in the linearization order.
-- Fails if the given shape does not have the same number of elements as the list.
-- O(1) time.
{-# INLINE fromVector #-}
fromVector :: forall sh v a . (HasCallStack, Vector v, VecElem v a, Shape sh) =>
              v a -> Array sh v a
fromVector v | n /= l = error $ "fromVector: size mismatch " ++ show (n, l)
             | otherwise = A $ T st 0 v
  where n : st = getStridesT ss
        l = vLength v
        ss = shapeP (Proxy :: Proxy sh)

-- | Make sure the underlying vector is in the linearization order.
-- This is semantically an identity function, but can have big performance
-- implications.
-- O(n) or O(1) time.
{-# INLINE normalize #-}
normalize :: (Vector v, VecElem v a, Shape sh) => Array sh v a -> Array sh v a
normalize = fromVector . toVector

-- | Change the shape of an array.  Type error if the arrays have different number of elements.
-- O(n) or O(1) time.
{-# INLINE reshape #-}
reshape :: forall sh' sh v a .
           (Vector v, VecElem v a, Shape sh, Shape sh', Size sh ~ Size sh') =>
           Array sh v a -> Array sh' v a
reshape a = reshape' (shapeP (Proxy :: Proxy sh')) (shapeL a) a

reshape' :: (Vector v, VecElem v a) =>
            ShapeL -> ShapeL -> Array sh v a -> Array sh' v a
reshape' sh sh' (A t@(T ost oo v))
  | vLength v == 1 = A $ T (map (const 0) sh) 0 v  -- Fast special case for singleton vector
  | Just nst <- simpleReshape ost sh' sh = A $ T nst oo v
  | otherwise = A $ fromVectorT sh $ toVectorT sh' t

-- | Change the size of dimensions with size 1.  These dimension can be changed to any size.
-- All other dimensions must remain the same.
-- O(1) time.
{-# INLINE stretch #-}
stretch :: forall sh' sh v a . (Shape sh, Shape sh', ValidStretch sh sh') =>
           Array sh v a -> Array sh' v a
stretch = stretch' (stretching (Proxy :: Proxy sh) (Proxy :: Proxy sh'))

stretch' :: [Bool] -> Array sh v a -> Array sh' v a
stretch' str (A vs) = A $ stretchT str vs

-- | Change the size of the outermost dimension by replication.
{-# INLINE stretchOuter #-}
stretchOuter :: forall s sh v a . (Shape sh) =>
                Array (1 : sh) v a -> Array (s : sh) v a
stretchOuter (A vs) = A $ stretchT (True : map (const False) (strides vs)) vs

-- | Convert a value to a scalar (rank 0) array.
-- O(1) time.
{-# INLINE scalar #-}
scalar :: (Vector v, VecElem v a) => a -> Array '[] v a
scalar = A . scalarT

-- | Convert a scalar (rank 0) array to a value.
-- O(1) time.
{-# INLINE unScalar #-}
unScalar :: (Vector v, VecElem v a) => Array '[] v a -> a
unScalar (A t) = unScalarT t

-- | Make an array with all elements having the same value.
-- O(1) time.
{-# INLINE constant #-}
constant :: forall sh v a . (Vector v, VecElem v a, Shape sh) =>
            a -> Array sh v a
constant = A . constantT (shapeP (Proxy :: Proxy sh))

-- | Map over the array elements.
-- O(n) time.
{-# INLINE mapA #-}
mapA :: (Vector v, VecElem v a, VecElem v b, Shape sh) =>
        (a -> b) -> Array sh v a -> Array sh v b
mapA f a@(A t) = A $ mapT (shapeL a) f t

-- | Map over the array elements.
-- O(n) time.
{-# INLINE zipWithA #-}
zipWithA :: (Vector v, VecElem v a, VecElem v b, VecElem v c, Shape sh) =>
            (a -> b -> c) -> Array sh v a -> Array sh v b -> Array sh v c
zipWithA f a@(A t) (A t') = A $ zipWithT (shapeL a) f t t'

-- | Map over the array elements.
-- O(n) time.
{-# INLINE zipWith3A #-}
zipWith3A :: (Vector v, VecElem v a, VecElem v b, VecElem v c, VecElem v d, Shape sh) =>
             (a -> b -> c -> d) -> Array sh v a -> Array sh v b -> Array sh v c -> Array sh v d
zipWith3A f a@(A t) (A t') (A t'') = A $ zipWith3T (shapeL a) f t t' t''

-- | Pad each dimension on the low and high side with the given value.
-- O(n) time.
{-# INLINE pad #-}
pad :: forall ps sh' sh a v . (HasCallStack, Vector v, VecElem v a, Padded ps sh sh', Shape sh) =>
       a -> Array sh v a -> Array sh' v a
pad v a@(A at) = A $ snd $ padT v aps ash at
  where ash = shapeL a
        aps = padded (Proxy :: Proxy ps) (Proxy :: Proxy sh)

-- | Do an arbitrary array transposition.
-- Fails if the transposition argument is not a permutation of the numbers
-- [0..r-1], where r is the rank of the array.
-- O(1) time.
{-# INLINE transpose #-}
transpose :: forall is sh v a .
             (Permutation is, Rank is <= Rank sh, Shape sh, Shape is, KnownNat (Rank sh)) =>
             Array sh v a -> Array (Permute is sh) v a
transpose (A t) = A (transposeT is' t)
  where l = length is
        n = valueOf @(Rank sh)
        is' = is ++ [l .. n-1]
        is = shapeP (Proxy :: Proxy is)

-- | Append two arrays along the outermost dimension.
-- All dimensions, except the outermost, must be the same.
-- O(n) time.
{-# INLINE append #-}
append :: (Vector v, VecElem v a, Shape sh, KnownNat m, KnownNat n, KnownNat (m+n)) =>
          Array (m ': sh) v a -> Array (n ': sh) v a -> Array (m+n ': sh) v a
append a b = fromVector (vAppend (toVector a) (toVector b))

-- | Turn a rank-1 array of arrays into a single array by making the outer array into the outermost
-- dimension of the result array.  All the arrays must have the same shape.
-- O(n) time.
{-# INLINE ravel #-}
ravel :: (Vector v, Vector v', VecElem v a, VecElem v' (Array sh v a)
         , Shape sh, KnownNat s) =>
         Array '[s] v' (Array sh v a) -> Array (s:sh) v a
ravel = fromVector . vConcat . map toVector . toList

-- | Turn an array into a nested array, this is the inverse of 'ravel'.
-- I.e., @ravel . unravel == id@.
-- O(n) time.
{-# INLINE unravel #-}
unravel :: (Vector v, Vector v', VecElem v a, VecElem v' (Array sh v a)
           , Shape sh, KnownNat s) =>
           Array (s:sh) v a -> Array '[s] v' (Array sh v a)
unravel = rerank @1 scalar

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
{-# INLINE window #-}
window :: forall ws sh' sh v a .
          (Window ws sh sh', Vector v, KnownNat (Rank ws)) =>
          Array sh v a -> Array sh' v a
window (A (T ss o v)) = A (T (ss' ++ ss) o v)
  where ss' = take (valueOf @(Rank ws)) ss

-- | Stride the outermost dimensions.
-- E.g., if the array shape is @[10,12,8]@ and the strides are
-- @[2,2]@ then the resulting shape will be @[5,6,8]@.
-- O(1) time.
{-# INLINE stride #-}
stride :: forall ts sh' sh v a .
          (Stride ts sh sh', Vector v, Shape ts) =>
          Array sh v a -> Array sh' v a
stride (A (T ss o v)) = A (T (zipWith (*) (ats ++ repeat 1) ss) o v)
  where ats = shapeP (Proxy :: Proxy ts)

-- | Extract a slice of an array.
-- The first type argument is a list of (offset, length) pairs.
-- The length of the slicing argument must not exceed the rank of the array.
-- The extracted slice must fall within the array dimensions.
-- E.g. @slice @'[ '(1,2)] (fromList @'[4] [1,2,3,4]) == fromList @'[2] [2,3]@.
-- O(1) time.
{-# INLINE slice #-}
slice :: forall sl sh' sh v a .
         (Slice sl sh sh') =>
         Array sh v a -> Array sh' v a
slice (A (T ts o v)) = A (T ts (o+i) v)
  where i = sum $ zipWith (*) ts $ sliceOffsets (Proxy :: Proxy sl) (Proxy :: Proxy sh)

-- | Apply a function to the subarrays /n/ levels down and make
-- the results into an array with the same /n/ outermost dimensions.
-- The /n/ must not exceed the rank of the array.
-- O(n) time.
{-# INLINE rerank #-}
rerank :: forall n i o sh v v' a b .
          (Vector v, Vector v', VecElem v a, VecElem v' b,
           Drop n sh ~ i, Shape sh, KnownNat n, Shape o, Shape (Take n sh ++ o)) =>
          (Array i v a -> Array o v' b) -> Array sh v a -> Array (Take n sh ++ o) v' b
rerank f a@(A t) =
  fromVector $
  vConcat $
  map (toVector . f . A) $
  subArraysT osh t
  where osh = take (valueOf @n) (shapeL a)

-- | Apply a two-argument function to the subarrays /n/ levels down and make
-- the results into an array with the same /n/ outermost dimensions.
-- The /n/ must not exceed the rank of the array.
-- O(n) time.
{-# INLINE rerank2 #-}
rerank2 :: forall n i1 i2 o sh1 sh2 r v a b c .
           (Vector v, VecElem v a, VecElem v b, VecElem v c,
            Drop n sh1 ~ i1, Drop n sh2 ~ i2, Shape sh1, Shape sh2,
            Take n sh1 ~ r, Take n sh2 ~ r,
            KnownNat n, Shape o, Shape (r ++ o)) =>
           (Array i1 v a -> Array i2 v b -> Array o v c) -> Array sh1 v a -> Array sh2 v b -> Array (r ++ o) v c
rerank2 f aa@(A ta) (A tb) =
  fromVector $
  vConcat $
  zipWith (\ a b -> toVector $ f (A a) (A b))
          (subArraysT osh ta)
          (subArraysT osh tb)
  where osh = take (valueOf @n) (shapeL aa)


-- | Reverse the given dimensions, with the outermost being dimension 0.
-- O(1) time.
{-# INLINE rev #-}
rev :: forall rs sh v a . (ValidDims rs sh, Shape rs, Shape sh) => Array sh v a -> Array sh v a
rev a@(A t) = A (reverseT rs sh t)
  where rs = shapeP (Proxy :: Proxy rs)
        sh = shapeL a

-- | Reduce all elements of an array into a rank 0 array.
-- To reduce parts use 'rerank' and 'transpose' together with 'reduce'.
-- O(n) time.
{-# INLINE reduce #-}
reduce :: (Vector v, VecElem v a, Shape sh) =>
          (a -> a -> a) -> a -> Array sh v a -> Array '[] v a
reduce f z a@(A t) = A $ reduceT (shapeL a) f z t

-- | Right fold across all elements of an array.
{-# INLINE foldrA #-}
foldrA
  :: (Vector v, VecElem v a, Shape sh)
  => (a -> b -> b) -> b -> Array sh v a -> b
foldrA f z a@(A t) = foldrT (shapeL a) f z t

-- | Constrained version of 'traverse' for 'Array's.
{-# INLINE traverseA #-}
traverseA
  :: (Vector v, VecElem v a, VecElem v b, Applicative f, Shape sh)
  => (a -> f b) -> Array sh v a -> f (Array sh v b)
traverseA f a@(A t) = A <$> traverseT (shapeL a) f t

-- | Check if all elements of the array are equal.
allSameA :: (Shape sh, Vector v, VecElem v a, Eq a) => Array sh v a -> Bool
allSameA a@(A t) = allSameT (shapeL a) t

instance (Shape sh, Vector v, VecElem v a, Arbitrary a) => Arbitrary (Array sh v a) where
  arbitrary = fromList <$> vector (sizeP (Proxy :: Proxy sh))

-- | Sum of all elements.
{-# INLINE sumA #-}
sumA :: (Vector v, VecElem v a, Num a, Shape sh) => Array sh v a -> a
sumA a@(A t) = sumT (shapeL a) t

-- | Product of all elements.
{-# INLINE productA #-}
productA :: (Vector v, VecElem v a, Num a, Shape sh) => Array sh v a -> a
productA a@(A t) = productT (shapeL a) t

-- | Maximum of all elements.
{-# INLINE maximumA #-}
maximumA :: (Vector v, VecElem v a, Ord a, Shape sh, 1 <= Size sh) => Array sh v a -> a
maximumA a@(A t) = maximumT (shapeL a) t

-- | Minimum of all elements.
{-# INLINE minimumA #-}
minimumA :: (Vector v, VecElem v a, Ord a, Shape sh, 1 <= Size sh) => Array sh v a -> a
minimumA a@(A t) = minimumT (shapeL a) t

-- | Test if the predicate holds for any element.
{-# INLINE anyA #-}
anyA :: (Vector v, VecElem v a, Shape sh) => (a -> Bool) -> Array sh v a -> Bool
anyA p a@(A t) = anyT (shapeL a) p t

-- | Test if the predicate holds for all elements.
{-# INLINE allA #-}
allA :: (Vector v, VecElem v a, Shape sh) => (a -> Bool) -> Array sh v a -> Bool
allA p a@(A t) = allT (shapeL a) p t

-- | Put the dimensions of the argument into the specified dimensions,
-- and just replicate the data along all other dimensions.
-- The list of dimensions indicies must have the same rank as the argument array
-- and it must be strictly ascending.
broadcast :: forall ds sh' sh v a .
             (Shape sh, Shape sh',
              Broadcast ds sh sh',
              Vector v, VecElem v a) =>
             Array sh v a -> Array sh' v a
broadcast a = stretch' bc $
              reshape' sh rsh a
  where sh' = shapeP (Proxy :: Proxy sh')
        sh = shapeP (Proxy :: Proxy sh)
        rsh = [ if b then 1 else s | (s, b) <- zip sh' bc ]
        bc = broadcasting @ds @sh @sh'

-- | Generate an array with a function that computes the value for each index.
{-# INLINE generate #-}
generate :: forall sh v a .
            (Vector v, VecElem v a, Shape sh) =>
            ([Int] -> a) -> Array sh v a
generate = A . generateT (shapeP (Proxy :: Proxy sh))

-- | Iterate a function n times.
{-# INLINE iterateN #-}
iterateN :: forall n v a .
            (Vector v, VecElem v a, KnownNat n) =>
            (a -> a) -> a -> Array '[n] v a
iterateN f = A . iterateNT (valueOf @n) f

-- | Generate a vector from 0 to n-1.
{-# INLINE iota #-}
iota :: forall n v a .
        (Vector v, VecElem v a, KnownNat n, Enum a, Num a) =>
        Array '[n] v a
iota = A $ iotaT (valueOf @n)
