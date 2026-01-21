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
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
module Data.Array.Internal(module Data.Array.Internal) where
import Control.DeepSeq
import Data.Data(Data)
import qualified Data.DList as DL
import Data.Kind (Type)
import Data.List(foldl', zipWith4, zipWith5, sortBy, sortOn, foldl1')
import Data.Proxy
import GHC.Exts(Constraint, build)
import GHC.Generics(Generic)
import GHC.TypeLits(KnownNat, natVal)
import Text.PrettyPrint
import Text.PrettyPrint.HughesPJClass

{- HLINT ignore "Reduce duplication" -}

-- The underlying storage of values must be an instance of Vector.
-- For some types, like unboxed vectors, we require an extra
-- constraint on the elements, which VecElem allows you to express.
-- For vector types that don't need the constraint it can be set
-- to some dummy class.
-- | The 'Vector' class is the interface to the underlying storage for the arrays.
-- The operations map straight to operations for 'Vector'.
class Vector v where
  type VecElem v :: Type -> Constraint
  vIndex    :: (VecElem v a) => v a -> Int -> a
  vLength   :: (VecElem v a) => v a -> Int
  vToList   :: (VecElem v a) => v a -> [a]
  vFromList :: (VecElem v a) => [a] -> v a
  vSingleton:: (VecElem v a) => a -> v a
  vReplicate:: (VecElem v a) => Int -> a -> v a
  vMap      :: (VecElem v a, VecElem v b) => (a -> b) -> v a -> v b
  vZipWith  :: (VecElem v a, VecElem v b, VecElem v c) => (a -> b -> c) -> v a -> v b -> v c
  vZipWith3 :: (VecElem v a, VecElem v b, VecElem v c, VecElem v d) => (a -> b -> c -> d) -> v a -> v b -> v c -> v d
  vZipWith4 :: (VecElem v a, VecElem v b, VecElem v c, VecElem v d, VecElem v e) => (a -> b -> c -> d -> e) -> v a -> v b -> v c -> v d -> v e
  vZipWith5 :: (VecElem v a, VecElem v b, VecElem v c, VecElem v d, VecElem v e, VecElem v f) => (a -> b -> c -> d -> e -> f) -> v a -> v b -> v c -> v d -> v e -> v f
  vAppend   :: (VecElem v a) => v a -> v a -> v a
  vConcat   :: (VecElem v a) => [v a] -> v a
  vFold     :: (VecElem v a) => (a -> a -> a) -> a -> v a -> a
  vSlice    :: (VecElem v a) => Int -> Int -> v a -> v a
  vSum      :: (VecElem v a, Num a) => v a -> a
  vProduct  :: (VecElem v a, Num a) => v a -> a
  vMaximum  :: (VecElem v a, Ord a) => v a -> a
  vMinimum  :: (VecElem v a, Ord a) => v a -> a
  vUpdate   :: (VecElem v a) => v a -> [(Int, a)] -> v a
  vGenerate :: (VecElem v a) => Int -> (Int -> a) -> v a
  vAll      :: (VecElem v a) => (a -> Bool) -> v a -> Bool
  vAny      :: (VecElem v a) => (a -> Bool) -> v a -> Bool

class None a
instance None a

-- This instance is not used anywheer.  It serves more as a reference semantics.
instance Vector [] where
  type VecElem [] = None
  vIndex = (!!)
  vLength = length
  vToList = id
  vFromList = id
  vSingleton = pure
  vReplicate = replicate
  vMap = map
  vZipWith = zipWith
  vZipWith3 = zipWith3
  vZipWith4 = zipWith4
  vZipWith5 = zipWith5
  vAppend = (++)
  vConcat = concat
  vFold = foldl'
  vSlice o n = take n . drop o
  vSum = sum
  vProduct = product
  vMaximum = maximum
  vMinimum = minimum
  vUpdate xs us = loop xs (sortOn fst us) 0
    where
      loop [] [] _ = []
      loop [] (_:_) _ = error "vUpdate: out of bounds"
      loop as [] _ = as
      loop (a:as) ias@((i,a'):ias') n =
        case compare i n of
          LT -> error "vUpdate: bad index"
          EQ -> a' : loop as ias' (n+1)
          GT -> a  : loop as ias  (n+1)
  vGenerate n f = map f [0 .. n-1]
  vAll = all
  vAny = any

prettyShowL :: (Pretty a) => PrettyLevel -> a -> String
prettyShowL l = render . pPrintPrec l 0

-- | The type /T/ is the internal type of arrays.  In general,
-- operations on /T/ do no sanity checking as that should be done
-- at the point of call.
--
-- To avoid manipulating the data the indexing into the vector containing
-- the data is somewhat complex.  To find where item /i/ of the outermost
-- dimension starts you calculate vector index @offset + i*strides[0]@.
-- To find where item /i,j/ of the two outermost dimensions is you
-- calculate vector index @offset + i*strides[0] + j*strides[1]@, etc.
type role T representational nominal
data T v a = T
    { strides :: ![Int]   -- length is tensor rank
    , offset  :: !Int     -- offset into vector of values
    , values  :: !(v a)   -- actual values
    }
    deriving (Show, Generic, Data)

instance NFData (v a) => NFData (T v a)

-- | The shape of an array is a list of its dimensions.
type ShapeL = [Int]

badShape :: ShapeL -> Bool
badShape = any (< 0)

-- When shapes match, we can be efficient and use loop-fused comparisons instead
-- of materializing a vector.
-- Note this assumes the shape is the same for both Vectors.
-- TODO(augustss): if the array is a small fraction of the vector this can be inefficient.
{-# INLINABLE equalT #-}
equalT :: (Vector v, VecElem v a, Eq a, Eq (v a))
                  => ShapeL -> T v a -> T v a -> Bool
equalT s x y | strides x == strides y
               && offset x == offset y
               && values x == values y = True
             | otherwise = toVectorT s x == toVectorT s y

-- Note this assumes the shape is the same for both Vectors.
{-# INLINABLE compareT #-}
compareT :: (Vector v, VecElem v a, Ord a, Ord (v a))
            => ShapeL -> T v a -> T v a -> Ordering
compareT s x y = compare (toVectorT s x) (toVectorT s y)

-- Given the dimensions, return the stride in the underlying vector
-- for each dimension.  The first element of the list is the total length.
{-# INLINE getStridesT #-}
getStridesT :: ShapeL -> [Int]
getStridesT = scanr (*) 1

-- Convert an array to a list by indexing through all the elements.
-- The first argument is the array shape.
-- XXX Copy special cases from Tensor.
{-# INLINE toListT #-}
toListT :: (Vector v, VecElem v a) => ShapeL -> T v a -> [a]
toListT sh a@(T ss0 o0 v)
  | isCanonicalT (getStridesT sh) a = vToList v
  | otherwise = build $ \cons nil ->
      -- TODO: because unScalarT uses vIndex, this has unnecessary bounds
      -- checks.  We should expose an unchecked indexing function in the Vector
      -- class, add top-level bounds checks to cover the full range we'll
      -- access, and then do all accesses with the unchecked version.
      let go []     ss o rest = cons (unScalarT (T ss o v)) rest
          go (n:ns) ss o rest = foldr
            (\i -> case indexT (T ss o v) i of T ss' o' _ -> go ns ss' o')
            rest
            [0..n-1]
      in  go sh ss0 o0 nil

-- | Check if the strides are canonical, i.e., if the vector have the natural layout.
-- XXX Copy special cases from Tensor.
{-# INLINE isCanonicalT #-}
isCanonicalT :: (Vector v, VecElem v a) => [Int] -> T v a -> Bool
isCanonicalT (n:ss') (T ss o v) =
    o == 0 &&         -- Vector offset is 0
    ss == ss' &&      -- All strides are normal
    vLength v == n    -- The vector is the right size
isCanonicalT _ _ = error "impossible"

-- Convert a value to a scalar array.
{-# INLINE scalarT #-}
scalarT :: (Vector v, VecElem v a) => a -> T v a
scalarT = T [] 0 . vSingleton

-- Convert a scalar array to the actual value.
{-# INLINE unScalarT #-}
unScalarT :: (Vector v, VecElem v a) => T v a -> a
unScalarT (T _ o v) = vIndex v o

-- Make a constant array.
{-# INLINE constantT #-}
constantT :: (Vector v, VecElem v a) => ShapeL -> a -> T v a
constantT sh x = T (map (const 0) sh) 0 (vSingleton x)

-- TODO: change to return a list of vectors.
-- Convert an array to a vector in the natural order.
{-# INLINE toVectorT #-}
toVectorT :: (Vector v, VecElem v a) => ShapeL -> T v a -> v a
toVectorT sh a@(T ats ao v) =
  let l : ts' = getStridesT sh
      -- Are strides ok from this point?
      oks = scanr (&&) True (zipWith (==) ats ts')
      loop _ [] _ o =
        DL.singleton (vSlice o 1 v)
      loop (b:bs) (s:ss) (t:ts) o =
        if b then
          -- All strides normal from this point,
          -- so just take a slice of the underlying vector.
          DL.singleton (vSlice o (s*t) v)
        else
          -- Strides are not normal, collect slices.
          DL.concat [ loop bs ss ts (i*t + o) | i <- [0 .. s-1] ]
      loop _ _ _ _ = error "impossible"
  in  if head oks && vLength v == l then
        -- All strides are normal, return entire vector
        v
      else if oks !! length sh then  -- Special case for speed.
        -- Innermost dimension is normal, so slices are non-trivial.
        vConcat $ DL.toList $ loop oks sh ats ao
      else
        -- All slices would have length 1, going via a list is faster.
        vFromList $ toListT sh a

-- Convert to a vector containing the right elements,
-- but not necessarily in the right order.
-- This is used for reduction with commutative&associative operations.
{-# INLINE toUnorderedVectorT #-}
toUnorderedVectorT :: (Vector v, VecElem v a) => ShapeL -> T v a -> v a
toUnorderedVectorT sh a@(T ats ao v) =
  -- Figure out if the array maps onto some contiguous slice of the vector.
  -- Do this by checking if a transposition of the array corresponds to
  -- normal strides.
  -- First sort the strides in descending order, and rearrange the shape the same way.
  -- Then compute the strides from this rearranged shape; these will be the normal
  -- strides for this shape.  If these strides agree with the sorted actual strides
  -- it is a transposition, and we can just slice out the relevant piece of the vector.
  let
    (ats', sh') = unzip $ sortBy (flip compare) $ zip ats sh
    l : ts' = getStridesT sh'
  in
      if ats' == ts' then
        vSlice ao l v
      else
        toVectorT sh a

-- Convert from a vector.
{-# INLINE fromVectorT #-}
fromVectorT :: ShapeL -> v a -> T v a
fromVectorT sh = T (tail $ getStridesT sh) 0

-- Convert from a list
{-# INLINE fromListT #-}
fromListT :: (Vector v, VecElem v a) => [Int] -> [a] -> T v a
fromListT sh = fromVectorT sh . vFromList

-- Index into the outermost dimension of an array.
{-# INLINE indexT #-}
indexT :: T v a -> Int -> T v a
indexT (T (s : ss) o v) i = T ss (o + i * s) v
indexT _ _ = error "impossible"

-- Stretch the given dimensions to have arbitrary size.
-- The stretched dimensions must have size 1, and stretching is
-- done by setting the stride to 0.
{-# INLINE stretchT #-}
stretchT :: [Bool] -> T v a -> T v a
stretchT bs (T ss o v) = T (zipWith (\ b s -> if b then 0 else s) bs ss) o v

-- Map over the array elements.
{-# INLINE mapT #-}
mapT :: (Vector v, VecElem v a, VecElem v b) => ShapeL -> (a -> b) -> T v a -> T v b
mapT sh f (T ss o v) | product sh >= vLength v = T ss o (vMap f v)
mapT sh f t = fromVectorT sh $ vMap f $ toVectorT sh t

-- Zip two arrays with a function.
{-# INLINE zipWithT #-}
zipWithT :: (Vector v, VecElem v a, VecElem v b, VecElem v c) =>
            ShapeL -> (a -> b -> c) -> T v a -> T v b -> T v c
zipWithT sh f t@(T ss _ v) t'@(T _ _ v') =
  case (vLength v, vLength v') of
    (1, 1) ->
      -- If both vectors have length 1, then it's a degenerate case and it's better
      -- to operate on the single element directly.
      T ss 0 $ vSingleton $ f (vIndex v 0) (vIndex v' 0)
    (1, _) ->
      -- First vector has length 1, so use a map instead.
      mapT sh (vIndex v 0 `f` ) t'
    (_, 1) ->
      -- Second vector has length 1, so use a map instead.
      mapT sh (`f` vIndex v' 0) t
    (_, _) ->
      let cv  = toVectorT sh t
          cv' = toVectorT sh t'
      in  fromVectorT sh $ vZipWith f cv cv'

-- Zip three arrays with a function.
{-# INLINE zipWith3T #-}
zipWith3T :: (Vector v, VecElem v a, VecElem v b, VecElem v c, VecElem v d) =>
             ShapeL -> (a -> b -> c -> d) -> T v a -> T v b -> T v c -> T v d
zipWith3T _ f (T ss _ v) (T _ _ v') (T _ _ v'') |
  -- If all vectors have length 1, then it's a degenerate case and it's better
  -- to operate on the single element directly.
  vLength v == 1, vLength v' == 1, vLength v'' == 1 =
    T ss 0 $ vSingleton $ f (vIndex v 0) (vIndex v' 0) (vIndex v'' 0)
zipWith3T sh f t t' t'' = fromVectorT sh $ vZipWith3 f v v' v''
  where v   = toVectorT sh t
        v'  = toVectorT sh t'
        v'' = toVectorT sh t''

-- Zip four arrays with a function.
{-# INLINE zipWith4T #-}
zipWith4T :: (Vector v, VecElem v a, VecElem v b, VecElem v c, VecElem v d, VecElem v e) => ShapeL -> (a -> b -> c -> d -> e) -> T v a -> T v b -> T v c -> T v d -> T v e
zipWith4T sh f t t' t'' t''' = fromVectorT sh $ vZipWith4 f v v' v'' v'''
  where v   = toVectorT sh t
        v'  = toVectorT sh t'
        v'' = toVectorT sh t''
        v'''= toVectorT sh t'''

-- Zip five arrays with a function.
{-# INLINE zipWith5T #-}
zipWith5T :: (Vector v, VecElem v a, VecElem v b, VecElem v c, VecElem v d, VecElem v e, VecElem v f) => ShapeL -> (a -> b -> c -> d -> e -> f) -> T v a -> T v b -> T v c -> T v d -> T v e -> T v f
zipWith5T sh f t t' t'' t''' t'''' = fromVectorT sh $ vZipWith5 f v v' v'' v''' v''''
  where v   = toVectorT sh t
        v'  = toVectorT sh t'
        v'' = toVectorT sh t''
        v'''= toVectorT sh t'''
        v''''= toVectorT sh t''''

-- Do an arbitrary transposition.  The first argument should be
-- a permutation of the dimension, i.e., the numbers [0..r-1] in some order
-- (where r is the rank of the array).
{-# INLINE transposeT #-}
transposeT :: [Int] -> T v a -> T v a
transposeT is (T ss o v) = T (permute is ss) o v

-- Return all subarrays n dimensions down.
-- The shape argument should be a prefix of the array shape.
{-# INLINE subArraysT #-}
subArraysT :: ShapeL -> T v a -> [T v a]
subArraysT sh ten = sub sh ten []
  where sub [] t = (t :)
        sub (n:ns) t = foldr (.) id [sub ns (indexT t i) | i <- [0..n-1]]

-- Reverse the given dimensions.
{-# INLINE reverseT #-}
reverseT :: [Int] -> ShapeL -> T v a -> T v a
reverseT rs sh (T ats ao v) = T rts ro v
  where (ro, rts) = rev 0 sh ats
        rev !_ [] [] = (ao, [])
        rev r (m:ms) (t:ts) | r `elem` rs = (o + (m-1)*t, -t : ts')
                            | otherwise   = (o,            t : ts')
          where (o, ts') = rev (r+1) ms ts
        rev _ _ _ = error "reverseT: impossible"

-- Reduction of all array elements.
{-# INLINE reduceT #-}
reduceT :: (Vector v, VecElem v a) =>
           ShapeL -> (a -> a -> a) -> a -> T v a -> T v a
reduceT sh f z = scalarT . vFold f z . toVectorT sh

-- Right fold via toListT.
{-# INLINE foldrT #-}
foldrT
  :: (Vector v, VecElem v a) => ShapeL -> (a -> b -> b) -> b -> T v a -> b
foldrT sh f z a = foldr f z (toListT sh a)

-- Traversal via toListT/fromListT.
{-# INLINE traverseT #-}
traverseT
  :: (Vector v, VecElem v a, VecElem v b, Applicative f)
  => ShapeL -> (a -> f b) -> T v a -> f (T v b)
traverseT sh f a = fmap (fromListT sh) (traverse f (toListT sh a))

-- Fast check if all elements are equal.
allSameT :: (Vector v, VecElem v a, Eq a) => ShapeL -> T v a -> Bool
allSameT sh t@(T _ _ v)
  | vLength v <= 1 = True
  | otherwise =
    let !v' = toVectorT sh t
        !x = vIndex v' 0
    in  vAll (x ==) v'

newtype Rect = Rect { unRect :: [String] }  -- A rectangle of text

toRect :: String -> Rect
toRect = Rect . lines

fromRect :: Rect -> String
fromRect (Rect ls) = unlines ls

-- Make each Rect be of size h * w
rectPad :: Int -> Int -> Rect -> Rect
rectPad h w (Rect ls) = Rect $ map padL ls ++ replicate (h - length ls) mt
  where mt = replicate w ' '
        padL s = replicate (w - length s) ' ' ++ s

-- Horizontal catenation.  Assumes input rectangle are padded.
-- Adds empty space between Rects.
hcatRect :: Rect -> Rect -> Rect
hcatRect (Rect xs) (Rect ys) = Rect $ zipWith (\ x y -> x ++ " " ++ y) xs ys

-- Vertical catenation.  Assumes input rectangle are padded.
-- Adds no space between Rects.
vcatRect :: Rect -> Rect -> Rect
vcatRect (Rect xs) (Rect ys) = Rect $ xs ++ ys

rectHeight :: Rect -> Int
rectHeight = length . unRect

-- Widest line
rectWidth :: Rect -> Int
rectWidth = maximum . (0:) . map length . unRect

ppT
  :: (Vector v, VecElem v a, Pretty a)
  => PrettyLevel -> Rational -> ShapeL -> T v a -> Doc
ppT l p sh = maybeParens (p > 10) . vcat' . map text . unRect . box boxMode . ppT_ (prettyShowL l) sh
  where boxMode | l >= prettyNormal = BoxMode True True True
                | otherwise = BoxMode False False False
        vcat' = foldl' ($+$) empty

ppT_
  :: (Vector v, VecElem v a)
  => (a -> String) -> ShapeL -> T v a -> Rect
ppT_ show_ sh t = showsT sh t'
  where ss = map (toRect . show_) $ toListT sh t
        maxH = maximum $ map rectHeight ss
        maxW = maximum $ map rectWidth ss
        ss' = map (rectPad maxH maxW) ss
        t' :: T [] Rect
        t' = T (tail (getStridesT sh)) 0 ss'

showsT :: [Int] -> T [] Rect -> Rect
showsT []     t = unScalarT t
showsT s@[_]  t = foldl1' hcatRect $ toListT s t
showsT (n:ns) t = foldl1' vcat' rs
  where vcat' x y = vcatRect x (vcatRect spc y)
        spc = Rect $ replicate (length ns - 1) (replicate (rectWidth (head rs)) ' ')
        rs = [ showsT ns (indexT t i) | i <- [0..n-1] ]

data BoxMode = BoxMode { _bmBars, _bmUnicode, _bmHeader :: Bool }

prettyBoxMode :: BoxMode
prettyBoxMode = BoxMode False False False

-- Possibly draw a box around a (padded) rectangle.
box :: BoxMode -> Rect -> Rect
box BoxMode{..} (Rect ls) =
  let bar | _bmUnicode = '\x2502'
          | otherwise = '|'
      dash | _bmUnicode = '\x2500'
           | otherwise = '-'
      ls' | _bmBars = map (\ l -> if null l then l else [bar] ++ l ++ [bar]) ls
          | otherwise = ls
      h = replicate (length (head ls)) dash
      t | _bmUnicode = "\x250c" ++ h ++ "\x2510"
        | otherwise = "+" ++ h ++ "+"
      b | _bmUnicode = "\x2514" ++ h ++ "\x2518"
        | otherwise = t
      ls'' | _bmHeader = [t] ++ ls' ++ [b]
           | otherwise = ls'
  in  Rect ls''

zipWithLong2 :: (a -> b -> b) -> [a] -> [b] -> [b]
zipWithLong2 f (a:as) (b:bs) = f a b : zipWithLong2 f as bs
zipWithLong2 _     _     bs  = bs

padT :: forall v a . (Vector v, VecElem v a) => a -> [(Int, Int)] -> ShapeL -> T v a -> ([Int], T v a)
padT v aps ash at = (ss, fromVectorT ss $ vConcat $ pad' aps ash st at)
  where pad' :: [(Int, Int)] -> ShapeL -> [Int] -> T v a -> [v a]
        pad' [] sh _ t = [toVectorT sh t]
        pad' ((l,h):ps) (s:sh) (n:ns) t =
          [vReplicate (n*l) v] ++ concatMap (pad' ps sh ns . indexT t) [0..s-1] ++ [vReplicate (n*h) v]
        pad' _ _ _ _ = error $ "pad: rank mismatch " ++ show (length aps, length ash)
        _ : st = getStridesT ss
        ss = zipWithLong2 (\ (l,h) s -> l+s+h) aps ash

-- Check if a reshape is just adding/removing some dimensions of
-- size 1, in which case it can be done by just manipulating
-- the strides.  Given the old strides, the old shapes, and the
-- new shape it will return the possible new strides.
simpleReshape :: [Int] -> ShapeL -> ShapeL -> Maybe [Int]
simpleReshape osts os ns
  | filter (1 /=) os == filter (1 /=) ns = Just $ loop ns sts'
    -- Old and new dimensions agree where they are not 1.
    where
      -- Get old strides for non-1 dimensions
      sts' = [ st | (st, s) <- zip osts os, s /= 1 ]
      -- Insert stride 0 for all 1 dimensions in new shape.
      loop [] [] = []
      loop (1:ss)     sts  = 0  : loop ss sts
      loop (_:ss) (st:sts) = st : loop ss sts
      loop _ _ = error $ "simpleReshape: shouldn't happen " ++ show (osts, os, ns)
simpleReshape _ _ _ = Nothing

-- Note: assumes + is commutative&associative.
{-# INLINE sumT #-}
sumT :: (Vector v, VecElem v a, Num a) => ShapeL -> T v a -> a
sumT sh = vSum . toUnorderedVectorT sh

-- Note: assumes * is commutative&associative.
{-# INLINE productT #-}
productT :: (Vector v, VecElem v a, Num a) => ShapeL -> T v a -> a
productT sh = vProduct . toUnorderedVectorT sh

-- Note: assumes max is commutative&associative.
{-# INLINE maximumT #-}
maximumT :: (Vector v, VecElem v a, Ord a) => ShapeL -> T v a -> a
maximumT sh = vMaximum . toUnorderedVectorT sh

-- Note: assumes min is commutative&associative.
{-# INLINE minimumT #-}
minimumT :: (Vector v, VecElem v a, Ord a) => ShapeL -> T v a -> a
minimumT sh = vMinimum . toUnorderedVectorT sh

{-# INLINE anyT #-}
anyT :: (Vector v, VecElem v a) => ShapeL -> (a -> Bool) -> T v a -> Bool
anyT sh p = vAny p . toUnorderedVectorT sh

{-# INLINE allT #-}
allT :: (Vector v, VecElem v a) => ShapeL -> (a -> Bool) -> T v a -> Bool
allT sh p = vAll p . toUnorderedVectorT sh

{-# INLINE updateT #-}
updateT :: (Vector v, VecElem v a) => ShapeL -> T v a -> [([Int], a)] -> T v a
updateT sh t us = T ss 0 $ vUpdate (toVectorT sh t) $ map ix us
  where _ : ss = getStridesT sh
        ix (is, a) = (sum $ zipWith (*) is ss, a)

{-# INLINE generateT #-}
generateT :: (Vector v, VecElem v a) => ShapeL -> ([Int] -> a) -> T v a
generateT sh f = T ss 0 $ vGenerate s g
  where s : ss = getStridesT sh
        g i = f (toIx ss i)
        toIx [] _ = []
        toIx (n:ns) i = q : toIx ns r where (q, r) = quotRem i n

{-# INLINE iterateNT #-}
iterateNT :: (Vector v, VecElem v a) => Int -> (a -> a) -> a -> T v a
iterateNT n f x = fromListT [n] $ take n $ iterate f x

{-# INLINE iotaT #-}
iotaT :: (Vector v, VecElem v a, Enum a, Num a) => Int -> T v a
iotaT n = fromListT [n] [0 .. fromIntegral n - 1]    -- TODO: should use V.enumFromTo instead

-------

-- | Permute the elements of a list, the first argument is indices into the original list.
permute :: [Int] -> [a] -> [a]
permute is xs = map (xs!!) is

-- | Like 'dropWhile' but at the end of the list.
revDropWhile :: (a -> Bool) -> [a] -> [a]
revDropWhile p = reverse . dropWhile p . reverse

allSame :: (Eq a) => [a] -> Bool
allSame [] = True
allSame (x : xs) = all (x ==) xs

-- | Get the value of a type level Nat.
-- Use with explicit type application, i.e., @valueOf \@42@
{-# INLINE valueOf #-}
valueOf :: forall n i . (KnownNat n, Num i) => i
valueOf = fromInteger $ natVal (Proxy :: Proxy n)
