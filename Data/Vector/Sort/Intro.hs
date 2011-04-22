{-# LANGUAGE ImplicitParams #-}
module Data.Vector.Sort.Intro (sort, sortBy) where

import Data.Vector.Sort.Common
import Data.Vector.Sort.Quick.Pivot
import Data.Vector.Sort.Quick.Partition
import qualified Data.Vector.Sort.Heap.Binary as Heap

{-# INLINE sort #-}
sort :: (Vector v a, Ord a) => v a -> v a
sort = sortBy (<=)

{-# INLINE sortBy #-}
sortBy :: Vector v a => LEq a -> v a -> v a
sortBy = sortPermM sortM

sortM :: (?cmp :: Comparator) => PMVector s Elem -> ST s ()
sortM !arr = let
  sortImpl !depth
    | depth < depthLimit = 
	sequentialSort $ \ xs -> pickPivot xs $ 
	  \ pivotIndex -> partition pivotIndex xs $ 
	  \ breakIndex -> let
	      doLeft = sortImpl (depth + 1) (takeM breakIndex xs)
	      doRight = sortImpl (depth + 1) (dropM (breakIndex + 1) xs)
	      in if breakIndex * 2 <= lengthM xs then doRight >> doLeft
		  else doLeft >> doRight
    | otherwise = Heap.sortByM
  in sortImpl 0 arr
  where depthLimit = 2 * intLog n
	n = lengthM xs