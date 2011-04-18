{-# LANGUAGE ImplicitParams, BangPatterns #-}
module Data.Vector.Sort.Heap.Nary (sortBy) where

import Control.Monad

import Data.Bits
import qualified Data.Vector.Primitive as P
import Data.Vector.Sort.Common

import Prelude hiding (read)

arity :: Int
arity = 3

sortBy :: Vector v a => LEq a -> v a -> v a
sortBy = sortPermM sortByM

sortByM :: (?cmp :: Comparator) => PMVector s Elem -> ST s ()
sortByM = sequentialSort $ \ xs -> do
  heapify xs
  sortHeap xs

heapify :: (?cmp :: Comparator) => PMVector s Elem -> ST s ()
heapify xs = go_heapify ((lengthM xs - 1) `quot` arity) where
  go_heapify k = when (k >= 0) $ do
    siftTop xs k =<< read xs k
    go_heapify (k-1)

siftTop :: (?cmp :: Comparator) => PMVector s Elem -> Int -> Elem -> ST s ()
siftTop !xs k x
  | arity * k + 1 < lengthM xs = do
      minChild <- P.fold1M' (\ i j -> do
	  xi <- read xs i
	  xj <- read xs j
	  if xi <=? xj then return i else return j) (P.takeWhile (< lengthM xs) $ P.enumFromN (arity * k + 1) arity)
      xC <- read xs minChild
      if xC <=? x then write xs k x else write xs k xC >> siftTop xs xC x
  | otherwise = write xs k x

sortHeap :: (?cmp :: Comparator) => PMVector s Elem -> ST s ()
sortHeap xs = go_sort (lengthM xs - 1) where
  go_sort k = when (k > 0) $ do
    top <- read xs 0
    x <- read xs k
    write xs k top
    siftTop xs 0 x
    go_sort (k-1)