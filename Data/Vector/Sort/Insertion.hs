{-# LANGUAGE BangPatterns #-}
module Data.Vector.Sort.Insertion where

import Control.Monad
import Control.Monad.ST

import Data.Vector.Generic (modify, Vector, Mutable)
import Data.Vector.Generic.Mutable (MVector, length, unsafeWrite, unsafeRead)

import Prelude hiding (length)

{-# INLINE insertionSort #-}
insertionSort :: (Ord a, Vector v a) => v a -> v a
insertionSort = modify insertionSortM

{-# INLINE insertionSortM #-}
insertionSortM :: (Ord a, MVector v a) => v s a -> ST s ()
insertionSortM xs = run 1 where
  !n = length xs
  run !i = when (i < n) $ do
      x <- unsafeRead xs i
      let go (-1) = unsafeWrite xs 0 x
	  go j = do
	    y <- unsafeRead xs j
	    if y <= x then unsafeWrite xs (j+1) x else do
	      unsafeWrite xs (j+1) y
	      go (j-1)
      go (i-1)
      run (i+1)