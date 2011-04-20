{-# LANGUAGE ImplicitParams, BangPatterns #-}
module Data.Vector.Sort.Heap.Pairing (sortBy) where

import Data.Vector.Sort.Common
import Data.Vector.Generic (stream, unstream)
import Data.Vector.Sort.Heap.PHeap

import Prelude hiding (read, mapM, length)

{-# INLINE sortBy #-}
sortBy :: Vector v a => LEq a -> v a -> v a
sortBy (<=?) !xs = sortPerm (sortImpl (length xs)) (<=?) xs

{-# INLINE sortImpl #-}
sortImpl :: (?cmp :: Comparator) => Int -> PVector Int -> PVector Int
sortImpl n xs = unstream (unheap n (heapify (stream xs)))