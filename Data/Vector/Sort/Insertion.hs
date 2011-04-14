{-# LANGUAGE BangPatterns, ImplicitParams #-}
module Data.Vector.Sort.Insertion where

import Control.Monad

import Data.Vector.Sort.Types
import Data.Vector.Sort.Comparator

import Prelude hiding (length, read)

{-# INLINE sort #-}
sort :: (Ord a, Vector v a) => v a -> v a
sort = sortBy (<=)

{-# INLINE sortBy #-}
sortBy :: Vector v a => LEq a -> v a -> v a
sortBy = sortPermM sortByM

sortByM :: (?cmp :: Comparator) => PMVector s Int -> ST s ()
sortByM xs = run 1 where
  !n = lengthM xs
  run !i = when (i < n) $ do
      x <- read xs i
      let go j
	    | j < 0	= done 0
	    | otherwise = do
		y <- read xs j
		if y <=? x then done (j+1) else go (j-1)
	  done j = do
	    moveBy xs j (i-j) 1
	    write xs j x
	    run (i+1)
      go (i-1)