{-# LANGUAGE BangPatterns, ImplicitParams #-}
module Data.Vector.Sort.Quick where

import Debug.Trace

import Control.Monad.ST

import Data.Vector.Sort.Types
import Data.Vector.Sort.Comparator
import qualified Data.Vector.Sort.Insertion as Ins

import Prelude hiding (length, read)

{-# INLINE sort #-}
sort :: (Vector v a, Ord a) => v a -> v a
sort = sortBy (<=)

{-# INLINE sortBy #-}
sortBy :: Vector v a => LEq a -> v a -> v a
sortBy = sortPermM sortByM

{-# INLINE sortByM #-}
sortByM :: (?cmp :: Comparator) => PMVector s Int -> ST s ()
sortByM = let
  qSort xs
    | n <= 3	= Ins.sortByM xs
    | otherwise	= do
	let pivotIndex = 0
	pivot <- read xs pivotIndex
	let right = n - 1
	swap xs pivotIndex right
	let go !i !storeIndex = if i >= right then done storeIndex else do
	      x <- read xs i
	      if x <=? pivot then do
		  swap xs i storeIndex
		  go (i+1) (storeIndex+1)
	      else go (i+1) storeIndex
	    done !storeIndex = do
	      swap xs storeIndex right
	      let doLeft = qSort (takeM storeIndex xs)
		  doRight = qSort (dropM (storeIndex + 1) xs)
	      if storeIndex * 2 <= n then doRight >> doLeft
		else doLeft >> doRight
	go 0 0
    where n = lengthM xs
  in qSort

medOf3 :: LEq a -> a -> a -> a -> a
medOf3 (<=) a b c = case (a <= b, b <= c, a <= c) of
  (True, True, _)	-> b
  (True, False, True)	-> c
  (True, False, False)	-> a
  (False, _, True)	-> a
  (False, True, False)	-> c
  (False, False, _)	-> b