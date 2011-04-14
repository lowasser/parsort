{-# LANGUAGE ImplicitParams, BangPatterns, DoAndIfThenElse #-}
module Data.Vector.Sort.Parallel.Merge (sort, sortBy) where

import Control.Monad.Primitive

import Data.Bits

import Data.Vector.Sort.Common

import Data.Vector.Sort.Parallel.Utils
import Data.Vector.Sort.Merge.Inplace
import qualified Data.Vector.Sort.Merge as Seq

{-# INLINE sort #-}
sort :: (Vector v a, Ord a) => v a -> v a
sort = sortBy (<=)

{-# INLINE sortBy #-}
sortBy :: Vector v a => LEq a -> v a -> v a
sortBy = unsafeSortPermIO sortImpl

sortImpl :: (?cmp :: Comparator) => PMVector RealWorld Int -> IO ()
sortImpl = parallelSort Seq.sortImpl $ \ xs -> do
  let !n' = lengthM xs `shiftR` 1
  doBoth (sortImpl (takeM n' xs)) (sortImpl (dropM n' xs))
  primToPrim $ mergeLo n' xs