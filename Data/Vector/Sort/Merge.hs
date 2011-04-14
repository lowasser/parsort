{-# LANGUAGE ImplicitParams #-}
module Data.Vector.Sort.Merge where

import Data.Bits

import Data.Vector.Sort.Common
import Data.Vector.Sort.Merge.Inplace

{-# INLINE sort #-}
sort :: (Vector v a, Ord a) => v a -> v a
sort = sortBy (<=)

{-# INLINE sortBy #-}
sortBy :: Vector v a => LEq a -> v a -> v a
sortBy = sortPermM sortImpl

sortImpl :: (?cmp :: Comparator) => PMVector s Int -> ST s ()
sortImpl = sequentialSort $ \ xs -> do
  let n' = lengthM xs `shiftR` 1
  sortImpl (takeM n' xs)
  sortImpl (dropM n' xs)
  mergeLo n' xs