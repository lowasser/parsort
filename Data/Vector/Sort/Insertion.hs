{-# LANGUAGE BangPatterns #-}
module Data.Vector.Sort.Insertion where

import Control.Monad
import Control.Monad.Primitive

import Data.Vector.Generic (modify, Vector, Mutable)
import Data.Vector.Generic.Mutable (MVector, length, unsafeWrite, unsafeRead)

import Prelude hiding (length)

{-# INLINE sort #-}
sort :: (Ord a, Vector v a) => v a -> v a
sort = sortBy (<=)

{-# INLINE sortBy #-}
sortBy :: Vector v a => (a -> a -> Bool) -> v a -> v a
sortBy (<=) = modify (sortByM (<=))

{-# INLINE sortByM #-}
sortByM :: (PrimMonad m, MVector v a) => (a -> a -> Bool) -> v (PrimState m) a -> m ()
sortByM (<=?) xs = run 1 where
  !n = length xs
  run !i = when (i < n) $ do
      x <- unsafeRead xs i
      let go j
	    | j <= 0	= unsafeWrite xs 0 x
	    | otherwise = do
		y <- unsafeRead xs j
		if y <=? x then unsafeWrite xs (j+1) x else do
		  unsafeWrite xs (j+1) y
		  go (j-1)
      go (i-1)
      run (i+1)