{-# LANGUAGE ImplicitParams, BangPatterns #-}
module Data.Vector.Sort.Heap.Nary (sortBy) where

import Control.Monad

import Debug.Trace

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

maxIndex :: (?cmp :: Comparator) => PMVector s Elem -> Int -> Int -> ST s Int
maxIndex xs i j = do
  xi <- read xs i
  xj <- read xs j
  if xj <=? xi then return i else return j

siftTop :: (?cmp :: Comparator) => PMVector s Elem -> Int -> Elem -> ST s ()
siftTop !xs k x
  | childL < n = do
      minChild <- P.fold1M' (maxIndex xs) (P.enumFromN child1 arity)
      xC <- read xs minChild
      if xC <=? x then write xs k x else write xs k xC >> siftTop xs minChild x
  | child1 < n = do
      minChild <- P.fold1M' (maxIndex xs) (P.enumFromN child1 (n - child1))
      xC <- read xs minChild
      if xC <=? x then write xs k x else write xs k xC >> write xs minChild x
  | otherwise = write xs k x
  where n = lengthM xs
	child1 = arity * k + 1
	childL = arity * k + arity

sortHeap :: (?cmp :: Comparator) => PMVector s Elem -> ST s ()
sortHeap xs = go_sort (lengthM xs - 1) where
  go_sort k = when (k > 0) $ do
    top <- read xs 0
    x <- read xs k
    write xs k top
    siftTop (takeM k xs) 0 x
    go_sort (k-1)