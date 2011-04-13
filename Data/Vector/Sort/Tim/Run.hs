{-# LANGUAGE BangPatterns #-}
module Data.Vector.Sort.Tim.Run (countRunAndMakeAscending) where

import Data.Vector.Generic.Mutable (reverse)

import Data.Vector.Sort.Types

import Prelude hiding (length, read, reverse)

{-# SPECIALIZE countRunAndMakeAscending ::
      PrimMonad m => LEq Int -> PMVector (PrimState m) Int -> (Int -> m b) -> m b,
      PrimMonad m => LEq a => VMVector (PrimState m) a -> (Int -> m b) -> m b #-}
countRunAndMakeAscending :: (PrimMonad m, MVector v a) => 
  LEq a -> v (PrimState m) a -> (Int -> m b) -> m b
countRunAndMakeAscending (<=?) xs cont
  | n <= 1	= cont n
  | otherwise	= do
      x0 <- read xs 0
      x1 <- read xs 1
      if x0 <=? x1 then countAscendingRun x1 1 else countDescendingRun x1 1
  where	n = lengthM xs
	descCont k = do
	  reverse (takeM k xs)
	  cont k
	countAscendingRun xi !i
	  | i' < n	= do
	      xi' <- read xs i'
	      if xi <=? xi' then countAscendingRun xi' i' else cont i'
	  | otherwise	= cont n
	  where i' = i + 1
	countDescendingRun xi !i
	  | i' < n	= do
	      xi' <- read xs i'
	      if xi <=? xi' then descCont i' else countAscendingRun xi' i'
	  | otherwise	= descCont n
	  where i' = i + 1
