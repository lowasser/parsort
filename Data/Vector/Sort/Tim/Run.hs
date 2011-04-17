{-# LANGUAGE BangPatterns, DoAndIfThenElse, RecordWildCards, NamedFieldPuns, CPP, ScopedTypeVariables, ImplicitParams #-}
{-# OPTIONS -funbox-strict-fields #-}
module Data.Vector.Sort.Tim.Run (countRunAndMakeAscending) where

import Control.Monad.Primitive

import Data.Vector.Generic.Mutable (reverse)

import Data.Vector.Sort.Types
import Data.Vector.Sort.Comparator

import Prelude hiding (length, read, reverse, mapM_, take, drop)

{-# INLINE countRunAndMakeAscending #-}
countRunAndMakeAscending :: (?cmp :: Comparator) => PMVector RealWorld Int -> (Int -> IO b) -> IO b
countRunAndMakeAscending xs cont
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
	      if xi <=? xi' then descCont i' else countDescendingRun xi' i'
	  | otherwise	= descCont n
	  where i' = i + 1