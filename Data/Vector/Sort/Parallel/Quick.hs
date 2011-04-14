{-# LANGUAGE ImplicitParams #-}
module Data.Vector.Sort.Parallel.Quick (sort, sortBy) where

import Data.Vector.Sort.Common
import Data.Vector.Sort.Quick.Pivot
import Data.Vector.Sort.Quick.Partition
import Data.Vector.Sort.Parallel.Utils
import qualified Data.Vector.Sort.Quick as Seq

{-# INLINE sort #-}
sort :: (Vector v a, Ord a) => v a -> v a
sort = sortBy (<=)

{-# INLINE sortBy #-}
sortBy :: Vector v a => LEq a -> v a -> v a
sortBy = unsafeSortPermIO sortImpl

sortImpl :: (?cmp :: Comparator) => PMVector RealWorld Int -> IO ()
sortImpl = parallelSort Seq.sortImpl $ \ xs -> pickPivot xs $ 
  \ pivotIndex -> partition pivotIndex xs $
  \ breakIndex -> let 
      doLeft = sortImpl (takeM breakIndex xs)
      doRight = sortImpl (dropM (breakIndex + 1) xs)
      in doBoth doLeft doRight