{-# LANGUAGE ImplicitParams, BangPatterns #-}
module Data.Vector.Sort.Merge.Inplace where

import Control.Monad.ST

import Data.Vector.Generic.Mutable (clone)

import Data.Vector.Sort.Types
import Data.Vector.Sort.Comparator

import Prelude hiding (length, read, take, drop)

-- | Assumes that [0, k) and [k, n) are sorted, and merges them.
-- Assumes that k <= n/2.
mergeLo :: (?cmp :: Comparator) => Int -> PMVector s Int -> ST s ()
mergeLo k xs = do
  run1 <- clone (takeM k xs)
  let run2 = dropM k xs
  let !n1 = lengthM run1; !n2 = lengthM run2
  let go !i1 !i2
	| i1 >= n1	= return ()
	| i2 >= n2	= copyM (dropM iWrite xs) (dropM i1 run1)
	| otherwise	= do
	    x1 <- read run1 i1
	    x2 <- read run2 i2
	    if x1 <=? x2 then do
	      write xs iWrite x1
	      go (i1+1) i2
	    else do
	      write xs iWrite x2
	      go i1 (i2+1)
	where iWrite = i1 + i2
  go 0 0