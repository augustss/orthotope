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
module Data.Array.Dynamic.MatMul(matMul, convolve) where
import GHC.Stack(HasCallStack)
import Data.Array.Dynamic

-- | Simple (and slow) matrix multiply.
matMul :: (HasCallStack, Num a) => Array a -> Array a -> Array a
matMul x y | [m, n] <- shX, [n', o] <- shY, n == n' =
  generate [m, o] $ \ [i, j] ->
    let xv = reshape [n] $ slice [(i, 1), (0, n)] x
        yv = reshape [n] $ slice [(0, n), (j, 1)] y
    in  sum $ zipWithA (*) xv yv
           | otherwise =
  error $ "matMul: bad shapes: " ++
          show shX ++ " * " ++ show shY
  where shX = shapeL x
        shY = shapeL y


-- | Convolve the /n/ outer dimensions with the given kernel.
-- There is no padding nor striding.
-- The input has shape /spatialSh/ ++ /channelSh/,
-- the kernel has shape /spatialKernelSh/ ++ /channelSh/ ++ /featureSh/,
-- and the result has shape /spatialOutSh/  ++ /featureSh/.
-- The /n/ gives the rank of the /spatialSh/.
--
-- Example:
-- @
--  i :: Array {-[20,30,3]-} T  -- 20x30 image with 3 channels
--  k :: Array {-[5,5,3,8]-} T  -- 5x5 kernel with 8 output features
--  convolve 2 i k :: Array {-[16,26,8]-} T
-- @
convolve :: (HasCallStack, Num a) =>
            Int -> Array a -> Array a -> Array a
convolve n a k =
  let i = rank a
      ksh = shapeL k
      ws = take n ksh
      iw = window ws a
      wsh = shapeL iw
      ksc = product (take i ksh)            -- spatial + channels
      ksf = product (drop i ksh)            -- features
      isp = product (take n wsh)            -- spatial
      iwc = product (drop n wsh)            -- kernel + channels
      osh = take n wsh ++ drop i ksh
      ir = reshape [isp, iwc] iw
      kr = reshape [ksc, ksf] k
      m  = matMul ir kr
      r  = reshape osh m
  in  r
