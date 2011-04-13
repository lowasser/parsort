{-# LANGUAGE BangPatterns, ScopedTypeVariables, MultiParamTypeClasses, FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
module Data.Vector.Sort.Insertion.Binary where

import Control.Monad
import Control.Monad.Primitive

import Data.Bits

import Data.Vector.Sort.Types

import Prelude hiding (length, take, drop, read)

{-# SPECIALIZE sort :: 
      PVector Int -> PVector Int,
      Ord a => VVector a -> VVector a #-}
{-# INLINE sort #-}
sort :: (Vector v a, Movable (Mutable v) a, Ord a) => v a -> v a
sort = sortBy (<=)

{-# INLINE sortBy #-}
sortBy :: (Vector v a, Movable (Mutable v) a) => (a -> a -> Bool) -> v a -> v a
sortBy (<=) = modify (sortByM (<=))

{-# INLINE sortByM #-}
sortByM :: (Movable v a, PrimMonad m) => (a -> a -> Bool) -> v (PrimState m) a -> m ()
sortByM (<=?) xs = run (n-1) where
  !n = lengthM xs
  binarySearch key cont = bin where
    bin l u
      | u <= l	= cont l
      | otherwise = do
	  let k = (u + l) `shiftR` 1
	  x <- read xs k
	  if x <=? key then bin (k+1) u else bin l k
  run off = when (off > 0) $ do
    x <- read xs (off - 1)
    let insertAt i = do
	  let moveSrc = dropM off (takeM i xs)
	  let moveDst = dropM (off-1) (takeM (i-1) xs)
	  move moveDst moveSrc
	  write xs (i-1) x
    binarySearch x insertAt off n
    run (off-1)