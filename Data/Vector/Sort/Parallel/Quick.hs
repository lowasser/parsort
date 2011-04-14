{-# LANGUAGE BangPatterns, ImplicitParams #-}
module Data.Vector.Sort.Parallel.Quick where

import Control.Monad.Primitive

import Data.Vector.Sort.Types
import Data.Vector.Sort.Comparator
import Data.Vector.Sort.Quick.Pivot
import Data.Vector.Sort.Quick.Partition
import Data.Vector.Sort.Parallel.Utils
import qualified Data.Vector.Sort.Quick as Q

import Prelude hiding (length, read)

{-# INLINE sort #-}
sort :: (Vector v a, Ord a) => v a -> v a
sort = sortBy (<=)

{-# INLINE sortBy #-}
sortBy :: Vector v a => LEq a -> v a -> v a
sortBy (<=?) xs = unsafePerformIO $ sortPermIO sortByM (<=?) xs

{-# INLINE sortByM #-}
sortByM :: (?cmp :: Comparator) => PMVector RealWorld Int -> IO ()
sortByM xs
    | n <= 1000	= primToPrim $ Q.sortByM xs
    | otherwise	= pickPivot xs $ \ pivotIndex ->
	partition pivotIndex xs $ \ breakIndex -> 
	let doLeft = sortByM (takeM breakIndex xs)
	    doRight = sortByM (dropM (breakIndex + 1) xs)
	in doBoth doLeft doRight
    where n = lengthM xs