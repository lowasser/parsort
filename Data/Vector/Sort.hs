module Data.Vector.Sort (
  LEq,
  -- * /O(n log n)/ sorts
  mergeSortBy,
  heapSortBy,
  timSortBy,
  -- * Typically /O(n log n)/ sorts
  quickSortBy,
  binInsertionSortBy,
  -- * /O(n^2)/ sorts
  insertionSortBy,
  -- * Other sorts
  radixSort,
  Radix(..)
  ) where

import qualified Data.Vector.Sort.Merge as Merge
import qualified Data.Vector.Sort.Heap.Binary as Heap
import qualified Data.Vector.Sort.Quick as Quick
import qualified Data.Vector.Sort.Tim as Tim
import qualified Data.Vector.Sort.Insertion as Insertion
import qualified Data.Vector.Sort.Insertion.Binary as BinInsertion
import qualified Data.Vector.Sort.Radix as Radix

{-# INLINE quickSortBy #-}
-- | Sorts the specified vector using a quicksort algorithm, with median-of-nine pivot selection.
-- Typically takes /O(n log n)/, takes /O(n^2)/ in the worst case.  Not stable.
quickSortBy :: Vector v a => LEq a -> v a -> v a
quickSortBy = Quick.sortBy

{-# INLINE mergeSortBy #-}
-- | Sorts the specified vector using a merge sort algorithm, which is guaranteed to run in /O(n log n)/ time.  Stable.
mergeSortBy :: Vector v a => LEq a -> v a -> v a
mergeSortBy = Merge.sortBy

{-# INLINE heapSortBy #-}
-- | Sorts the specified vector using a binary heap sort algorithm, which is guaranteed to run in /O(n log n)/ time.  Not stable.
heapSortBy :: Vector v a => LEq a -> v a -> v a
heapSortBy = Heap.sortBy

{-# INLINE timSortBy #-}
-- | Sorts the specified vector using the timsort algorithm, which is guaranteed to run in /O(n log n)/ time.  Stable.
-- Timsort performs in practice as well as mergesort for random data, but performs significantly better on data 
-- with some preexisting order, even reverse order.
timSortBy :: Vector v a => LEq a -> v a -> v a
timSortBy = Tim.sortBy

{-# INLINE insertionSortBy #-}
-- | Sorts the specified vector using insertion sort, which runs in /O(n^2)/ time.  Stable.
-- Insertion sort approaches /O(n)/ on data that is already mostly sorted.
insertionSortBy :: Vector v a => LEq a -> v a -> v a
insertionSortBy = Insertion.sortBy

{-# INLINE binInsertionSortBy #-}
-- | Sorts the specified vector using a binary insertion sort, which runs in /O(n^2)/ time
-- but is guaranteed to make /&Theta;(n log n)/ comparisons.  Stable.  Approaches /O(n log n)/
-- on data that is already mostly sorted.
binInsertionSortBy :: Vector v a => LEq a -> v a -> v a
binInsertionSortBy = BinInsertion.sortBy

{-# INLINE radixSort #-}
-- | Sorts the specified vector using radix sort, which runs in /O(nw)/ time, where /w/
-- is the number of machine words needed to represent a single element.
radixSort :: (Vector v a, Radix a) => v a -> v a
radixSort = Radix.sort