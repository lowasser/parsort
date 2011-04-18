{-# LANGUAGE ImplicitParams, BangPatterns #-}
module Data.Vector.Sort.Heap.Nary (sortBy, sortByM) where

import Control.Monad

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

maxInRange :: (?cmp :: Comparator) => PMVector s Elem -> Int -> Int -> (Int -> Elem -> ST s a) -> ST s a
maxInRange !xs !l !r cont = do
  xl <- read xs l
  go l xl (l+1)
  where	go !c !xc !i
	  | i <= r = do	xi <- read xs i
			if xi <=? xc then go c xc (i+1) else go i xi (i+1)
	  | otherwise = cont c xc

siftTop :: (?cmp :: Comparator) => PMVector s Elem -> Int -> Elem -> ST s ()
siftTop !xs k x
  | child1 < n = maxInRange xs child1 (min (n-1) childL) $ \ !c !xC ->
      if xC <=? x then write xs k x else write xs k xC >> siftTop xs c x
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