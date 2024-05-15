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
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.Array.Internal.Shape(module Data.Array.Internal.Shape) where
import Data.Proxy
import Type.Reflection
import GHC.TypeLits

import Data.Array.Internal(valueOf)

type DivRoundUp  n m = Div (n+m-1) m

-----------------
-- Type level properties.

-- | Compute the rank, i.e., length of a type level shape.
type family Rank (s :: [Nat]) :: Nat where
  Rank '[] = 0
  Rank (n : ns) = 1 + Rank ns

-- | Compute the size, i.e., total number of elements of a type level shape.
type Size (s :: [Nat]) = Size' 1 s
type family Size' (a :: Nat) (s :: [Nat]) :: Nat where
  Size' a '[] = a
  Size' a (n : ns) = Size' (a * n) ns
-- Using an accumulating parameter generates fewer constraints.

----------------------------------
-- Type level shape operations

type family (++) (xs :: [Nat]) (ys :: [Nat]) :: [Nat] where
  (++) '[] ys = ys
  (++) (x ': xs) ys = x ': (xs ++ ys)

{-
-- XXX O(n^2)
type family Reverse (xs :: [Nat]) :: [Nat] where
    Reverse '[] = '[]
    Reverse (x ': xs) = Reverse xs ++ '[x]
-}

type family Take (n :: Nat) (xs :: [Nat]) :: [Nat] where
    Take 0 xs = '[]
    Take n (x ': xs) = x ': Take (n-1) xs

type family Drop (n :: Nat) (xs :: [Nat]) :: [Nat] where
    Drop 0 xs = xs
    Drop n (x ': xs) = Drop (n-1) xs

type family Last (xs :: [Nat]) where
  Last '[x] = x
  Last (x ': xs) = Last xs

type family Init (xs :: [Nat]) where
  Init '[x] = '[]
  Init (x ': xs) = x ': Init xs

-----------------

class ValidStretch (from :: [Nat]) (to :: [Nat]) where
  stretching :: Proxy from -> Proxy to -> [Bool]
instance ValidStretch '[] '[] where
  stretching _ _ = []
instance (BoolVal (Stretch s m), ValidStretch ss ms) => ValidStretch (s ': ss) (m ': ms) where
  stretching _ _ = boolVal (Proxy :: Proxy (Stretch s m)) :
    stretching (Proxy :: Proxy ss) (Proxy :: Proxy ms)
type family Stretch (s::Nat) (m::Nat) :: Bool where
  Stretch 1 m = 'True
  Stretch m m = 'False
  Stretch s m = TypeError ('Text "Cannot stretch " ':<>: 'ShowType s ':<>: 'Text " to " ':<>: 'ShowType m)

class BoolVal (b :: Bool) where
  boolVal :: Proxy b -> Bool
instance BoolVal 'False where
  boolVal _ = False
instance BoolVal 'True where
  boolVal _ = True

-----------------

class Padded (ps :: [(Nat, Nat)]) (sh :: [Nat]) (sh' :: [Nat]) | ps sh -> sh' where
  padded :: Proxy ps -> Proxy sh -> [(Int, Int)]
instance Padded '[] sh sh where
  padded _ _ = []
instance (KnownNat l, KnownNat h, (l+s+h) ~ s', Padded ps sh sh') =>
         Padded ('(l,h) ': ps) (s ': sh) (s' ': sh') where
  padded _ _ = (valueOf @l, valueOf @h) : padded (Proxy :: Proxy ps) (Proxy :: Proxy sh)

-----------------

class Permutation (is :: [Nat])
instance (AllElem is (Count 0 is), AllElem (Count 0 is) is) => Permutation is

type family Count (i :: Nat) (xs :: [Nat]) :: [Nat] where
  Count i '[] = '[]
  Count i (x ': xs) = i ': Count (i+1) xs

class AllElem (is :: [Nat]) (ns :: [Nat])
instance AllElem '[] ns
instance (Elem i ns, AllElem is ns) => AllElem (i ': is) ns
class Elem (i :: Nat) (ns :: [Nat])
instance (Elem' (CmpNat i n) i ns) => Elem i (n : ns)
class Elem' (e :: Ordering) (i :: Nat) (ns :: [Nat])
instance Elem' 'EQ i ns
instance (Elem i ns) => Elem' 'LT i ns
instance (Elem i ns) => Elem' 'GT i ns

type Permute (is :: [Nat]) (xs :: [Nat]) = Permute' is (Take (Rank is) xs) ++ Drop (Rank is) xs

type family Permute' (is :: [Nat]) (xs :: [Nat]) where
  Permute' '[] xs = '[]
  Permute' (i ': is) xs = Index xs i ': Permute' is xs

type family Index (xs :: [Nat]) (i :: Nat) where
  Index (x : xs) 0 = x
  Index (x : xs) i = Index xs (i-1)

class ValidDims (rs :: [Nat]) (sh :: [Nat])
instance (AllElem rs (Count 0 sh)) => ValidDims rs sh

-----------------

class Window (ws :: [Nat]) (ss :: [Nat]) (rs :: [Nat]) | ws ss -> rs
instance (Window' ws ws ss rs) => Window ws ss rs

class Window' (ows :: [Nat]) (ws :: [Nat]) (ss :: [Nat]) (rs :: [Nat]) | ows ws ss -> rs
instance ((ows ++ ss) ~ rs) => Window' ows '[] ss rs
instance (Window' ows ws ss rs, w <= s, ((s+1)-w) ~ r) => Window' ows (w ': ws) (s ': ss) (r ': rs)

-----------------

class Stride (ts :: [Nat]) (ss :: [Nat]) (rs :: [Nat]) | ts ss -> rs
instance Stride '[] ss ss
instance (Stride ts ss rs, DivRoundUp s t ~ r) => Stride (t ': ts) (s ': ss) (r ': rs)

-----------------

class Slice (ls :: [(Nat,Nat)]) (ss :: [Nat]) (rs :: [Nat]) | ls ss -> rs where
  sliceOffsets :: Proxy ls -> Proxy ss -> [Int]
instance Slice '[] ss ss where
  sliceOffsets _ _ = []
instance (Slice ls ss rs, (o+n) <= s, KnownNat o) => Slice ('(o,n) ': ls) (s ': ss) (n ': rs) where
  sliceOffsets _ _ = valueOf @o : sliceOffsets (Proxy :: Proxy ls) (Proxy :: Proxy ss)


-----------------
-- Shape extraction

class (Typeable s) => Shape (s :: [Nat]) where
  shapeP :: Proxy s -> [Int]
  sizeP  :: Proxy s -> Int

instance Shape '[] where
  {-# INLINE shapeP #-}
  shapeP _ = []
  {-# INLINE sizeP #-}
  sizeP  _ = 1

instance forall n s . (Shape s, KnownNat n) => Shape (n ': s) where
  {-# INLINE shapeP #-}
  shapeP _ = valueOf @n : shapeP (Proxy :: Proxy s)
  {-# INLINE sizeP #-}
  sizeP  _ = valueOf @n * sizeP  (Proxy :: Proxy s)

{-# INLINE shapeT #-}
shapeT :: forall sh . (Shape sh) => [Int]
shapeT = shapeP (Proxy :: Proxy sh)

{-# INLINE sizeT #-}
sizeT :: forall sh . (Shape sh) => Int
sizeT = sizeP (Proxy :: Proxy sh)

-- | Turn a dynamic shape back into a type level shape.
-- @withShape sh shapeP == sh@
withShapeP :: [Int] -> (forall sh . (Shape sh) => Proxy sh -> r) -> r
withShapeP [] f = f (Proxy :: Proxy ('[] :: [Nat]))
withShapeP (n:ns) f =
  case someNatVal (toInteger n) of
    Just (SomeNat (_ :: Proxy n)) -> withShapeP ns (\ (_ :: Proxy ns) -> f (Proxy :: Proxy (n ': ns)))
    _ -> error $ "withShape: bad size " ++ show n

withShape :: [Int] -> (forall sh . (Shape sh) => r) -> r
withShape sh f = withShapeP sh (\ (_ :: Proxy sh) -> f @sh)

-----------------

-- | Using the dimension indices /ds/, can /sh/ be broadcast into shape /sh'/?
class Broadcast (ds :: [Nat]) (sh :: [Nat]) (sh' :: [Nat]) where
  broadcasting :: [Bool]
instance (Broadcast' 0 ds sh sh') => Broadcast ds sh sh' where
  broadcasting = broadcasting' @0 @ds @sh @sh'

class Broadcast' (i :: Nat) (ds :: [Nat]) (sh :: [Nat]) (sh' :: [Nat]) where
  broadcasting' :: [Bool]
instance Broadcast' i '[] '[] '[] where
  broadcasting' = []
instance (Broadcast' i '[] '[] sh') => Broadcast' i '[] '[] (s : sh') where
  broadcasting' = True : broadcasting' @i @'[] @'[] @sh'
instance (TypeError ('Text "Too few dimension indices")) => Broadcast' i '[] (s ': sh) sh' where
  broadcasting' = undefined
instance (TypeError ('Text "Too many dimensions indices")) => Broadcast' i (d ': ds) '[] sh' where
  broadcasting' = undefined
instance (TypeError ('Text "Too few result dimensions")) => Broadcast' i (d ': ds) (s ': sh) '[] where
  broadcasting' = undefined
instance (Broadcast'' (CmpNat i d) i d ds (s ': sh) (s' ': sh')) => Broadcast' i (d ': ds) (s ': sh) (s' ': sh') where
  broadcasting' = broadcasting'' @(CmpNat i d) @i @d @ds @(s ': sh) @(s' ': sh')

class Broadcast'' (o :: Ordering) (i :: Nat) (d :: Nat) (ds :: [Nat]) (sh :: [Nat]) (sh' :: [Nat]) where
  broadcasting'' :: [Bool]
instance (Broadcast' (i+1)       ds  sh rsh) => Broadcast'' 'EQ i d ds (s ': sh) (s ': rsh) where
  broadcasting'' = False : broadcasting' @(i+1) @ds @sh @rsh
instance (Broadcast' (i+1) (d ': ds) sh rsh) => Broadcast'' 'LT i d ds sh (s' ': rsh) where
  broadcasting'' = True : broadcasting' @(i+1) @(d ': ds) @sh @rsh
instance (TypeError ('Text "unordered dimensions")) => Broadcast'' 'GT i d ds sh rsh where
  broadcasting'' = undefined
