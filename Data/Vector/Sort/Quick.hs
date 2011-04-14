{-# LANGUAGE BangPatterns, ImplicitParams #-}
module Data.Vector.Sort.Quick where

import Debug.Trace

import Control.Monad.ST

import Data.Vector.Sort.Types
import Data.Vector.Sort.Comparator
import Data.Vector.Sort.Quick.Pivot
import Data.Vector.Sort.Quick.Partition
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
    | n <= 20	= Ins.sortByM xs
    | otherwise	= pickPivot xs $ \ pivotIndex -> 
	partition pivotIndex xs $ \ breakIndex ->
	    let doLeft = qSort (takeM breakIndex xs)
		doRight = qSort (dropM (breakIndex + 1) xs)
	    in if breakIndex * 2 <= n then doRight >> doLeft
		else doLeft >> doRight
    where n = lengthM xs
  in qSort
