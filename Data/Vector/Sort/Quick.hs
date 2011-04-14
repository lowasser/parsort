{-# LANGUAGE BangPatterns, ImplicitParams #-}
module Data.Vector.Sort.Quick where

import Control.Monad.ST

import Data.Vector.Sort.Common
import Data.Vector.Sort.Quick.Pivot
import Data.Vector.Sort.Quick.Partition

{-# INLINE sort #-}
sort :: (Vector v a, Ord a) => v a -> v a
sort = sortBy (<=)

{-# INLINE sortBy #-}
sortBy :: Vector v a => LEq a -> v a -> v a
sortBy = sortPermM sortImpl

sortImpl :: (?cmp :: Comparator) => PMVector s Int -> ST s ()
sortImpl = sequentialSort $ \ xs -> pickPivot xs $ 
  \ pivotIndex -> partition pivotIndex xs $ 
  \ breakIndex -> let
      doLeft = sortImpl (takeM breakIndex xs)
      doRight = sortImpl (dropM (breakIndex + 1) xs)
      in if breakIndex * 2 <= lengthM xs then doRight >> doLeft
	  else doLeft >> doRight
