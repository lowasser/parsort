{-# LANGUAGE ImplicitParams, BangPatterns #-}
module Data.Vector.Sort.Heap.Binary (sortBy, sortByM) where

import Control.Monad

import Data.Vector.Sort.Common

import Prelude hiding (read)

sortBy :: Vector v a => LEq a -> v a -> v a
sortBy = sortPermM sortByM

sortByM :: (?cmp :: Comparator) => PMVector s Elem -> ST s ()
sortByM = sequentialSort $ \ xs -> do
  heapify xs
  sortHeap xs

heapify :: (?cmp :: Comparator) => PMVector s Elem -> ST s ()
heapify xs = go_heapify ((lengthM xs) `quot` 2) where
  go_heapify k = when (k >= 0) $ do
    siftTop xs k =<< read xs k
    go_heapify (k-1)

siftTop :: (?cmp :: Comparator) => PMVector s Elem -> Int -> Elem -> ST s ()
siftTop !xs k !x
  | c2 < n = do
      x1 <- read xs c1
      x2 <- read xs c2
      if x2 <=? x1 then siftChild c1 x1 else siftChild c2 x2
  | c2 == n = do
      let c = c1
      xC <- read xs c
      if xC <=? x then write xs k x else write xs k xC >> write xs c x
  | otherwise = write xs k x
  where n = lengthM xs
	c1 = 2 * k + 1
	c2 = 2 * k + 2
	siftChild !c !xC = if xC <=? x then write xs k x else write xs k xC >> siftTop xs c x

sortHeap :: (?cmp :: Comparator) => PMVector s Elem -> ST s ()
sortHeap xs = go_sort (lengthM xs - 1) where
  go_sort k = when (k > 0) $ do
    top <- read xs 0
    x <- read xs k
    write xs k top
    siftTop (takeM k xs) 0 x
    go_sort (k-1)