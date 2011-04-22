{-# LANGUAGE ScopedTypeVariables, BangPatterns #-}
module Data.Vector.Sort.Radix (sort, sortWith, Radix(..)) where

import Data.Vector.Sort.Radix.Class
import Data.Vector.Sort.Radix.Pass
import Data.Vector.Sort.Types

import Data.Vector.Generic (convert, stream, unstream)
import Data.Vector.Primitive (enumFromN)

import Data.Int
import Data.Word
import Prelude hiding (length)

{-# INLINE sort #-}
sort :: (Radix a, Vector v a) => v a -> v a
sort arr = convert (sortRadix (convert arr))

{-# INLINE sortWith #-}
sortWith :: (Radix r, Vector v a) => (a -> r) -> v a -> v a
sortWith key arr = backpermute arr $ sortRadixWith (unstream $ fmap key $ stream arr)

{-# SPECIALIZE sortRadix ::
      PVector Int -> PVector Int #-}
sortRadix :: forall a . Radix a => PVector a -> PVector a
sortRadix = sort_pass 0 where
  sort_pass p !xs
    | p < passes (undefined :: a)
	= sort_pass (p+1) $ radixPass (radix p) xs
    | otherwise = xs

{-# SPECIALIZE sortRadixWith ::
      PVector Int -> PVector Int,
      PVector Word -> PVector Int #-}
sortRadixWith :: forall a . Radix a => PVector a -> PVector Int
sortRadixWith !radixes = sort_pass 0 (enumFromN 0 n) where
  n = length radixes
  sort_pass p !xs
    | p < passes (undefined :: a)
        = sort_pass (p+1) $ radixPass (\ i -> radix p (index radixes i)) xs
    | otherwise = xs
