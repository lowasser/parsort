{-# LANGUAGE ScopedTypeVariables, BangPatterns #-}
module Data.Vector.Sort.Parallel.Radix (sort, sortWith, Radix(..)) where

import Data.Vector.Sort.Radix.Class
import Data.Vector.Sort.Parallel.Radix.Pass
import Data.Vector.Sort.Types

import Data.Vector.Generic (convert, stream, unstream)
import Data.Vector.Primitive (enumFromN)

import Data.Int
import Data.Word
import Prelude hiding (length)
import System.IO.Unsafe

{-# INLINE sort #-}
sort :: (Radix a, Vector v a) => v a -> v a
sort arr = convert (unsafePerformIO (sortRadix (convert arr)))

{-# INLINE sortWith #-}
sortWith :: (Radix r, Vector v a) => (a -> r) -> v a -> v a
sortWith key arr = backpermute arr $ unsafePerformIO $ sortRadixWith (unstream $ fmap key $ stream arr)

{-# SPECIALIZE sortRadix ::
      PVector Int -> IO (PVector Int) #-}
sortRadix :: forall a . Radix a => PVector a -> IO (PVector a)
sortRadix = sort_pass 0 where
  sort_pass p !xs
    | p < passes (undefined :: a)
	= sort_pass (p+1) =<< radixPass (radix p) xs
    | otherwise = return xs

{-# SPECIALIZE sortRadixWith ::
      PVector Int -> IO (PVector Int),
      PVector Word -> IO (PVector Int) #-}
sortRadixWith :: forall a . Radix a => PVector a -> IO (PVector Int)
sortRadixWith !radixes = sort_pass 0 (enumFromN 0 n) where
  n = length radixes
  sort_pass p !xs
    | p < passes (undefined :: a)
        = sort_pass (p+1) =<< radixPass (\ i -> radix p (index radixes i)) xs
    | otherwise = return xs
