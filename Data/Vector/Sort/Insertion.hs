{-# LANGUAGE BangPatterns, FlexibleContexts #-}
module Data.Vector.Sort.Insertion where

import Control.Monad
import Control.Monad.Primitive

import Data.Vector.Generic (modify, Vector, Mutable)
import Data.Vector.Generic.Mutable

import Data.Vector.Generic.Mutable.Move

import Prelude hiding (length)

{-# INLINE sort #-}
sort :: (Ord a, Vector v a, Movable (Mutable v) a) => v a -> v a
sort = sortBy (<=)

{-# INLINE sortBy #-}
sortBy :: (Vector v a, Movable (Mutable v) a) => (a -> a -> Bool) -> v a -> v a
sortBy (<=) = modify (sortByM (<=))

{-# INLINE sortByM #-}
sortByM :: (PrimMonad m, Movable v a) => (a -> a -> Bool) -> v (PrimState m) a -> m ()
sortByM (<=?) xs = run 1 where
  !n = length xs
  run !i = when (i < n) $ do
      x <- unsafeRead xs i
      let go j
	    | j < 0	= done 0
	    | otherwise = do
		y <- unsafeRead xs j
		if y <=? x then done (j+1) else go (j-1)
	  done j = do
	    let movSrc = unsafeDrop j (unsafeTake i xs)
	    let movDst = unsafeDrop (j+1) (unsafeTake (i+1) xs)
	    unsafeMove movDst movSrc
	    unsafeWrite xs j x
      go (i-1)
      run (i+1)