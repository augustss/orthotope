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
-- | Arrays of dynamic size, but static rank.  The arrays are polymorphic in the underlying
-- linear data structure used to store the actual values.
module Data.Array.Internal.RankedG(
  Array(..), Vector, VecElem,
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
  sumA, productA, maximumA, minimumA,
  anyA, allA,
  broadcast,
  generate, iterateN, iota,
  ) where
import Control.Monad(replicateM)
import Control.DeepSeq
import Data.Data(Data)
import Data.List(sort)
import GHC.Generics(Generic)
import GHC.Stack
import GHC.TypeLits(Nat, type (+), KnownNat, type (<=))
import Test.QuickCheck hiding (generate)
import Text.PrettyPrint.HughesPJClass hiding ((<>))

import Data.Array.Internal

-- | Arrays stored in a /v/ with values of type /a/.
type role Array nominal representational nominal
data Array (n :: Nat) v a = A !ShapeL !(T v a)
  deriving (Generic, Data)

instance (Vector v, Show a, VecElem v a) => Show (Array n v a) where
  {-# INLINABLE showsPrec #-}
  showsPrec p a@(A s _) = showParen (p > 10) $
    showString "fromList " . showsPrec 11 s . showString " " . showsPrec 11 (toList a)

instance (KnownNat n, Vector v, Read a, VecElem v a) => Read (Array n v a) where
  readsPrec p = readParen (p > 10) $ \ r1 ->
    [(fromList s xs, r4)
    | ("fromList", r2) <- lex r1, (s, r3) <- readsPrec 11 r2
    , (xs, r4) <- readsPrec 11 r3, length s == valueOf @n, product s == length xs]

instance (Vector v, Eq a, VecElem v a, Eq (v a)) => Eq (Array n v a) where
  (A s v) == (A s' v') = s == s' && equalT s v v'
  {-# INLINE (==) #-}

instance (Vector v, Ord a, Ord (v a), VecElem v a) => Ord (Array n v a) where
  (A s v) `compare` (A s' v') = compare s s' <> compareT s v v'
  {-# INLINE compare #-}

instance (Vector v, Pretty a, VecElem v a) => Pretty (Array n v a) where
  pPrintPrec l p (A sh t) = ppT l p sh t

instance (NFData (v a)) => NFData (Array n v a) where
  rnf (A sh v) = rnf sh `seq` rnf v

-- | The number of elements in the array.
-- O(1) time.
{-# INLINE size #-}
size :: Array n v a -> Int
size = product . shapeL

-- | The shape of an array, i.e., a list of the sizes of its dimensions.
-- In the linearization of the array the outermost (i.e. first list element)
-- varies most slowly.
-- O(1) time.
{-# INLINE shapeL #-}
shapeL :: Array n v a -> ShapeL
shapeL (A s _) = s

-- | The rank of an array, i.e., the number of dimensions it has.
-- O(1) time.
{-# INLINE rank #-}
rank :: forall n v a . (KnownNat n) => Array n v a -> Int
rank (A _ _) = valueOf @n

-- | Index into an array.  Fails if the array has rank 0 or if the index is out of bounds.
-- O(1) time.
{-# INLINE index #-}
index :: (Vector v, HasCallStack) => Array (1+n) v a -> Int -> Array n v a
index (A (s:ss) t) i | i < 0 || i >= s = error $ "index: out of bounds " ++ show (i, s)
                     | otherwise = A ss $ indexT t i
index (A [] _) _ = error "index: scalar"

-- | Convert to a list with the elements in the linearization order.
-- O(n) time.
{-# INLINE toList #-}
toList :: (Vector v, VecElem v a) => Array n v a -> [a]
toList (A sh t) = toListT sh t

-- | Convert to a vector with the elements in the linearization order.
-- O(n) or O(1) time (the latter if the vector is already in the linearization order).
{-# INLINE toVector #-}
toVector :: (Vector v, VecElem v a) => Array n v a -> v a
toVector (A sh t) = toVectorT sh t

-- | Convert from a list with the elements given in the linearization order.
-- Fails if the given shape does not have the same number of elements as the list.
-- O(n) time.
{-# INLINE fromList #-}
fromList :: forall n v a . (HasCallStack, Vector v, VecElem v a, KnownNat n) =>
            ShapeL -> [a] -> Array n v a
fromList ss vs | n /= l = error $ "fromList: size mismatch " ++ show (n, l)
               | length ss /= valueOf @n = error $ "fromList: rank mismatch " ++ show (length ss, valueOf @n :: Int)
               | otherwise = A ss $ T st 0 $ vFromList vs
  where n : st = getStridesT ss
        l = length vs

-- | Convert from a vector with the elements given in the linearization order.
-- Fails if the given shape does not have the same number of elements as the list.
-- O(1) time.
{-# INLINE fromVector #-}
fromVector :: forall n v a . (HasCallStack, Vector v, VecElem v a, KnownNat n) =>
              ShapeL -> v a -> Array n v a
fromVector ss v | n /= l = error $ "fromVector: size mismatch" ++ show (n, l)
                | length ss /= valueOf @n = error $ "fromVector: rank mismatch " ++ show (length ss, valueOf @n :: Int)
                | otherwise = A ss $ T st 0 v
  where n : st = getStridesT ss
        l = vLength v

-- | Make sure the underlying vector is in the linearization order.
-- This is semantically an identity function, but can have big performance
-- implications.
-- O(n) or O(1) time.
{-# INLINE normalize #-}
normalize :: (Vector v, VecElem v a, KnownNat n) => Array n v a -> Array n v a
normalize a = fromVector (shapeL a) $ toVector a

-- | Change the shape of an array.  Fails if the arrays have different number of elements.
-- O(n) or O(1) time.
{-# INLINE reshape #-}
reshape :: forall n n' v a . (HasCallStack,Vector v, VecElem v a, KnownNat n, KnownNat n') =>
           ShapeL -> Array n v a -> Array n' v a
reshape sh (A sh' t@(T ost oo v))
  | n /= n' = error $ "reshape: size mismatch " ++ show (sh, sh')
  | length sh /= valueOf @n' = error $ "reshape: rank mismatch " ++ show (length sh, valueOf @n' :: Int)
  | vLength v == 1 = A sh $ T (map (const 0) sh) 0 v  -- Fast special case for singleton vector
  | Just nst <- simpleReshape ost sh' sh = A sh $ T nst oo v
  | otherwise = A sh $ T st 0 $ toVectorT sh' t
  where n : st = getStridesT sh
        n' = product sh'

-- | Change the size of dimensions with size 1.  These dimension can be changed to any size.
-- All other dimensions must remain the same.
-- O(1) time.
{-# INLINE stretch #-}
stretch :: (HasCallStack) => ShapeL -> Array n v a -> Array n v a
stretch sh (A sh' vs) | Just bs <- str sh sh' = A sh $ stretchT bs vs
                      | otherwise = error $ "stretch: incompatible " ++ show (sh, sh')
  where str [] [] = Just []
        str (x:xs) (y:ys) | x == y = (False :) <$> str xs ys
                          | y == 1 = (True  :) <$> str xs ys
        str _ _ = Nothing

-- | Change the size of the outermost dimension by replication.
{-# INLINE stretchOuter #-}
stretchOuter :: (HasCallStack, 1 <= n) =>
                Int -> Array n v a -> Array n v a
stretchOuter s (A (1:sh) vs) =
  A (s:sh) $ stretchT (True : map (const False) (strides vs)) vs
stretchOuter _ _ = error "stretchOuter: needs outermost dimension of size 1"

-- | Convert a value to a scalar (rank 0) array.
-- O(1) time.
{-# INLINE scalar #-}
scalar :: (Vector v, VecElem v a) => a -> Array 0 v a
scalar = A [] . scalarT

-- | Convert a scalar (rank 0) array to a value.
-- O(1) time.
{-# INLINE unScalar #-}
unScalar :: (Vector v, VecElem v a) => Array 0 v a -> a
unScalar (A _ t) = unScalarT t

-- | Make an array with all elements having the same value.
-- O(1) time
{-# INLINE constant #-}
constant :: forall n v a . (Vector v, VecElem v a, KnownNat n) =>
            ShapeL -> a -> Array n v a
constant sh | badShape sh = error $ "constant: bad shape: " ++ show sh
            | length sh /= valueOf @n = error "constant: rank mismatch"
            | otherwise   = A sh . constantT sh

-- | Map over the array elements.
-- O(n) time.
{-# INLINE mapA #-}
mapA :: (Vector v, VecElem v a, VecElem v b) =>
        (a -> b) -> Array n v a -> Array n v b
mapA f (A s t) = A s (mapT s f t)

-- | Map over the array elements.
-- O(n) time.
{-# INLINE zipWithA #-}
zipWithA :: (Vector v, VecElem v a, VecElem v b, VecElem v c) =>
            (a -> b -> c) -> Array n v a -> Array n v b -> Array n v c
zipWithA f (A s t) (A s' t') | s == s' = A s (zipWithT s f t t')
                             | otherwise = error $ "zipWithA: shape mismatch: " ++ show (s, s')

-- | Map over the array elements.
-- O(n) time.
{-# INLINE zipWith3A #-}
zipWith3A :: (Vector v, VecElem v a, VecElem v b, VecElem v c, VecElem v d) =>
             (a -> b -> c -> d) -> Array n v a -> Array n v b -> Array n v c -> Array n v d
zipWith3A f (A s t) (A s' t') (A s'' t'') | s == s' && s == s'' = A s (zipWith3T s f t t' t'')
                                          | otherwise = error $ "zipWith3A: shape mismatch: " ++ show (s, s', s'')

-- | Pad each dimension on the low and high side with the given value.
-- O(n) time.
{-# INLINE pad #-}
pad :: forall n a v . (Vector v, VecElem v a) =>
       [(Int, Int)] -> a -> Array n v a -> Array n v a
pad aps v (A ash at) = uncurry A $ padT v aps ash at

-- | Do an arbitrary array transposition.
-- Fails if the transposition argument is not a permutation of the numbers
-- [0..r-1], where r is the rank of the array.
-- O(1) time.
{-# INLINE transpose #-}
transpose :: forall n v a . (KnownNat n) =>
            [Int] -> Array n v a -> Array n v a
transpose is (A sh t) | l > n = error "transpose: rank exceeded"
                      | sort is /= [0 .. l-1] =
                          error $ "transpose: not a permutation: " ++ show is
                      | otherwise = A (permute is' sh) (transposeT is' t)
  where l = length is
        n = valueOf @n
        is' = is ++ [l .. n-1]

-- | Append two arrays along the outermost dimension.
-- All dimensions, except the outermost, must be the same.
-- O(n) time.
{-# INLINE append #-}
append :: (Vector v, VecElem v a, KnownNat n) =>
          Array n v a -> Array n v a -> Array n v a
append a@(A (sa:sh) _) b@(A (sb:sh') _) | sh == sh' =
  fromVector (sa+sb : sh) (vAppend (toVector a) (toVector b))
append _ _ = error "append: bad shape"

-- | Concatenate a number of arrays into a single array.
-- Fails if any, but the outer, dimensions differ.
-- O(n) time.
{-# INLINE concatOuter #-}
concatOuter :: (Vector v, VecElem v a, KnownNat n) => [Array n v a] -> Array n v a
concatOuter [] = error "concatOuter: empty list"
concatOuter as | not $ allSame $ map tail shs =
                 error $ "concatOuter: non-conforming inner dimensions: " ++ show shs
               | otherwise = fromVector sh' $ vConcat $ map toVector as
  where shs@(sh:_) = map shapeL as
        sh' = sum (map head shs) : tail sh

-- | Turn a rank-1 array of arrays into a single array by making the outer array into the outermost
-- dimension of the result array.  All the arrays must have the same shape.
-- O(n) time.
{-# INLINE ravel #-}
ravel :: (Vector v, Vector v', VecElem v a, VecElem v' (Array n v a), KnownNat (1+n)) =>
         Array 1 v' (Array n v a) -> Array (1+n) v a
ravel aa =
  case toList aa of
    [] -> error "ravel: empty array"
    as | not $ allSame shs -> error $ "ravel: non-conforming inner dimensions: " ++ show shs
       | otherwise -> fromVector sh' $ vConcat $ map toVector as
      where shs@(sh:_) = map shapeL as
            sh' = length as : sh

-- | Turn an array into a nested array, this is the inverse of 'ravel'.
-- I.e., @ravel . unravel == id@.
{-# INLINE unravel #-}
unravel :: (Vector v, Vector v', VecElem v a, VecElem v' (Array n v a)) =>
           Array (1+n) v a -> Array 1 v' (Array n v a)
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
window :: forall n n' v a . (Vector v, KnownNat n, KnownNat n') =>
          [Int] -> Array n v a -> Array n' v a
window aws _ | valueOf @n' /= length aws + valueOf @n = error $ "window: rank mismatch: " ++ show (valueOf @n' :: Int, length aws, valueOf @n :: Int)
window aws (A ash (T ss o v)) = A (win aws ash) (T (ss' ++ ss) o v)
  where ss' = zipWith const ss aws
        win (w:ws) (s:sh) | w <= s = s - w + 1 : win ws sh
                          | otherwise = error $ "window: bad window size : " ++ show (w, s)
        win [] sh = aws ++ sh
        win _ _ = error $ "window: rank mismatch: " ++ show (aws, ash)

-- | Stride the outermost dimensions.
-- E.g., if the array shape is @[10,12,8]@ and the strides are
-- @[2,2]@ then the resulting shape will be @[5,6,8]@.
-- O(1) time.
{-# INLINE stride #-}
stride :: (Vector v) => [Int] -> Array n v a -> Array n v a
stride ats (A ash (T ss o v)) = A (str ats ash) (T (zipWith (*) (ats ++ repeat 1) ss) o v)
  where str (t:ts) (s:sh) = (s+t-1) `quot` t : str ts sh
        str [] sh = sh
        str _ _ = error $ "stride: rank mismatch: " ++ show (ats, ash)

-- | Rotate the array k times along the d'th dimension.
-- E.g., if the array shape is @[2, 3, 2]@, d is 1, and k is 4,
-- the resulting shape will be @[2, 4, 3, 2]@.
rotate :: forall d p v a.
          (KnownNat p, KnownNat d,
          Vector v, VecElem v a,
          -- Nonsense
          (d + (p + 1)) ~ ((p + d) + 1),
          (d + p) ~ (p + d),
          1 <= p + 1,
          KnownNat ((p + d) + 1),
          KnownNat (p + 1),
          KnownNat (1 + (p + 1))
          ) =>
          Int -> Array (p + d) v a -> Array (p + d + 1) v a
rotate k a = rerank @d @p @(p + 1) f a
 where
  f :: Array p v a -> Array (p + 1) v a
  f arr = let h:t = shapeL arr
              m = product t
              n = h * m
              arr' = reshape @p @(p + 1) (1:h:t) arr
              repeated = stretchOuter (k + 1) arr'
              flattened = reshape @(p + 1) @1 [(k + 1) * n] repeated
              batched = window @1 @2 [n] flattened
              strided = stride [n + m] batched
          in rev [0] (reshape (k:h:t) strided)

-- | Extract a slice of an array.
-- The first argument is a list of (offset, length) pairs.
-- The length of the slicing argument must not exceed the rank of the array.
-- The extracted slice must fall within the array dimensions.
-- E.g. @slice [1,2] (fromList [4] [1,2,3,4]) == [2,3]@.
-- O(1) time.
{-# INLINE slice #-}
slice :: [(Int, Int)] -> Array n v a -> Array n v a
slice asl (A ash (T ats ao v)) = A rsh (T ats o v)
  where (o, rsh) = slc asl ash ats
        slc ((k,n):sl) (s:sh) (t:ts) | k < 0 || k > s || k+n > s = error "slice: out of bounds"
                                     | otherwise = (i + k*t, n:ns) where (i, ns) = slc sl sh ts
        slc [] sh _ = (ao, sh)
        slc _ _ _ = error "impossible"

-- | Apply a function to the subarrays /n/ levels down and make
-- the results into an array with the same /n/ outermost dimensions.
-- The /n/ must not exceed the rank of the array.
-- O(n) time.
{-# INLINE rerank #-}
rerank :: forall n i o v v' a b .
          (Vector v, Vector v', VecElem v a, VecElem v' b
          , KnownNat n, KnownNat o, KnownNat (n+o), KnownNat (1+o)) =>
          (Array i v a -> Array o v' b) -> Array (n+i) v a -> Array (n+o) v' b
rerank f (A sh t) =
  ravelOuter osh $
  map (f . A ish) $
  subArraysT osh t
  where (osh, ish) = splitAt (valueOf @n) sh

{-# INLINABLE ravelOuter #-}
ravelOuter :: (Vector v, VecElem v a, KnownNat m) => ShapeL -> [Array n v a] -> Array m v a
ravelOuter _ [] = error "ravelOuter: empty list"
ravelOuter osh as | not $ allSame shs = error $ "ravelOuter: non-conforming inner dimensions: " ++ show shs
                  | otherwise = fromVector sh' $ vConcat $ map toVector as
  where shs@(sh:_) = map shapeL as
        sh' = osh ++ sh

-- | Apply a two-argument function to the subarrays /n/ levels down and make
-- the results into an array with the same /n/ outermost dimensions.
-- The /n/ must not exceed the rank of the array.
-- O(n) time.
{-# INLINE rerank2 #-}
rerank2 :: forall n i o a b c v .
           (Vector v, VecElem v a, VecElem v b, VecElem v c,
            KnownNat n, KnownNat o, KnownNat (n+o), KnownNat (1+o)) =>
           (Array i v a -> Array i v b -> Array o v c) -> Array (n+i) v a -> Array (n+i) v b -> Array (n+o) v c
rerank2 f (A sha ta) (A shb tb) | take n sha /= take n shb = error "rerank2: shape mismatch"
                                | otherwise =
  ravelOuter osh $
  zipWith (\ a b -> f (A isha a) (A ishb b))
          (subArraysT osh ta)
          (subArraysT osh tb)
  where (osh, isha) = splitAt n sha
        ishb = drop n shb
        n = valueOf @n

-- | Reverse the given dimensions, with the outermost being dimension 0.
-- O(1) time.
{-# INLINE rev #-}
rev :: [Int] -> Array n v a -> Array n v a
rev rs (A sh t) | all (\ r -> r >= 0 && r < n) rs = A sh (reverseT rs sh t)
                | otherwise = error "reverse: bad reverse dimension"
  where n = length sh

-- | Reduce all elements of an array into a rank 0 array.
-- To reduce parts use 'rerank' and 'transpose' together with 'reduce'.
-- O(n) time.
{-# INLINE reduce #-}
reduce :: (Vector v, VecElem v a) =>
          (a -> a -> a) -> a -> Array n v a -> Array 0 v a
reduce f z (A sh t) = A [] $ reduceT sh f z t

-- | Right fold across all elements of an array.
{-# INLINE foldrA #-}
foldrA :: (Vector v, VecElem v a) => (a -> b -> b) -> b -> Array n v a -> b
foldrA f z (A sh t) = foldrT sh f z t

-- | Constrained version of 'traverse' for 'Array's.
{-# INLINE traverseA #-}
traverseA
  :: (Vector v, VecElem v a, VecElem v b, Applicative f)
  => (a -> f b) -> Array n v a -> f (Array n v b)
traverseA f (A sh t) = A sh <$> traverseT sh f t

-- | Check if all elements of the array are equal.
allSameA :: (Vector v, VecElem v a, Eq a) => Array r v a -> Bool
allSameA (A sh t) = allSameT sh t

instance (KnownNat r, Vector v, VecElem v a, Arbitrary a) => Arbitrary (Array r v a) where
  arbitrary = do
    -- Don't generate huge number of elements
    ss <- replicateM (valueOf @r) (getSmall . getPositive <$> arbitrary) `suchThat` ((< 10000) . product)
    fromList ss <$> vector (product ss)

-- | Sum of all elements.
{-# INLINE sumA #-}
sumA :: (Vector v, VecElem v a, Num a) => Array r v a -> a
sumA (A sh t) = sumT sh t

-- | Product of all elements.
{-# INLINE productA #-}
productA :: (Vector v, VecElem v a, Num a) => Array r v a -> a
productA (A sh t) = productT sh t

-- | Maximum of all elements.
{-# INLINE maximumA #-}
maximumA :: (HasCallStack, Vector v, VecElem v a, Ord a) => Array r v a -> a
maximumA a@(A sh t) | size a > 0 = maximumT sh t
                    | otherwise  = error "maximumA called with empty array"

-- | Minimum of all elements.
{-# INLINE minimumA #-}
minimumA :: (HasCallStack, Vector v, VecElem v a, Ord a) => Array r v a -> a
minimumA a@(A sh t) | size a > 0 = minimumT sh t
                    | otherwise  = error "minimumA called with empty array"

-- | Test if the predicate holds for any element.
{-# INLINE anyA #-}
anyA :: (Vector v, VecElem v a) => (a -> Bool) -> Array r v a -> Bool
anyA p (A sh t) = anyT sh p t

-- | Test if the predicate holds for all elements.
{-# INLINE allA #-}
allA :: (Vector v, VecElem v a) => (a -> Bool) -> Array r v a -> Bool
allA p (A sh t) = allT sh p t

-- | Put the dimensions of the argument into the specified dimensions,
-- and just replicate the data along all other dimensions.
-- The list of dimensions indicies must have the same rank as the argument array
-- and it must be strictly ascending.
broadcast :: forall r' r v a .
             (HasCallStack, Vector v, VecElem v a, KnownNat r, KnownNat r') =>
             [Int] -> ShapeL -> Array r v a -> Array r' v a
broadcast ds sh a | length ds /= valueOf @r = error "broadcast: wrong number of broadcasts"
                  | any (\ d -> d < 0 || d >= r) ds = error "broadcast: bad dimension"
                  | not (ascending ds) = error "broadcast: unordered dimensions"
                  | length sh /= r = error "broadcast: wrong rank"
                  | otherwise = stretch sh $ reshape rsh a
  where r = valueOf @r'
        rsh = [ if i `elem` ds then s else 1 | (i, s) <- zip [0..] sh ]
        ascending (x:y:ys) = x < y && ascending (y:ys)
        ascending _ = True

-- | Generate an array with a function that computes the value for each index.
{-# INLINE generate #-}
generate :: forall n v a .
            (KnownNat n, Vector v, VecElem v a) =>
            ShapeL -> ([Int] -> a) -> Array n v a
generate sh | length sh /= valueOf @n = error $ "generate: rank mismatch " ++ show (length sh, valueOf @n :: Int)
            | otherwise = A sh . generateT sh

-- | Iterate a function n times.
{-# INLINE iterateN #-}
iterateN :: forall v a .
            (Vector v, VecElem v a) =>
            Int -> (a -> a) -> a -> Array 1 v a
iterateN n f = A [n] . iterateNT n f

-- | Generate a vector from 0 to n-1.
{-# INLINE iota #-}
iota :: forall v a .
        (Vector v, VecElem v a, Enum a, Num a) =>
        Int -> Array 1 v a
iota n = A [n] $ iotaT n
