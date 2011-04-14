{-# LANGUAGE ImplicitParams, BangPatterns #-}
module Data.Vector.Sort.Quick.Partition where

import Data.Vector.Sort.Types
import Data.Vector.Sort.Comparator

import Prelude hiding (read)

{-# INLINE partition #-}
partition :: (?cmp :: Comparator, PrimMonad m) => Int -> PMVector (PrimState m) Int -> (Int -> m a) -> m a
partition pivotIndex xs cont = do
  pivot <- read xs pivotIndex
  let right = lengthM xs - 1
  swap xs pivotIndex right
  let go !i !storeIndex 
	| i < right	= do
	    x <- read xs i
	    if x <=? pivot then do
		swap xs i storeIndex
		go (i+1) (storeIndex+1)
	    else go (i+1) storeIndex
	| otherwise	= do
	    swap xs storeIndex right
	    cont storeIndex
  go 0 0