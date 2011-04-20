{-# LANGUAGE BangPatterns, ImplicitParams #-}
{-# OPTIONS -funbox-strict-fields #-}
module Data.Vector.Sort.Heap.PHeap (heapify, unheap) where

import Data.Vector.Sort.Common
import Data.Vector.Fusion.Stream

data PHeap = PHeap !Elem PForest
data PForest = Cons !PHeap PForest | Nil

insert :: (?cmp :: Comparator) => PHeap -> Elem -> PHeap
insert t@(PHeap y ts) x
  | x <=? y   = PHeap x (Cons t Nil)
  | otherwise = PHeap y (Cons (PHeap x Nil) ts)

data PQueue = Empty | Queue !PHeap

combine :: (?cmp :: Comparator) => PForest -> PQueue
combine Nil = Empty
combine (Cons t Nil) = Queue t
combine (Cons t1 (Cons t2 ts)) = Queue (mergeAll (merge t1 t2) (merge2 ts))

mergeAll :: (?cmp :: Comparator) => PHeap -> PForest -> PHeap
mergeAll !t1 (Cons t2 ts) = mergeAll (merge t1 t2) ts
mergeAll t Nil = t

merge2 :: (?cmp :: Comparator) => PForest -> PForest
merge2 (Cons t1 (Cons t2 ts)) = Cons (merge t1 t2) (merge2 ts)
merge2 ts = ts

merge :: (?cmp :: Comparator) => PHeap -> PHeap -> PHeap
t1@(PHeap x1 ts1) `merge` t2@(PHeap x2 ts2)
  | x1 <=? x2 = PHeap x1 (Cons t2 ts1)
  | otherwise = PHeap x2 (Cons t1 ts2)

{-# INLINE heapify #-}
heapify :: (?cmp :: Comparator) => Stream Elem -> PQueue
heapify strm = foldl' insertQ Empty strm where
  insertQ Empty x = Queue (PHeap x Nil)
  insertQ (Queue q) x = Queue (insert q x)

{-# INLINE unheap #-}
unheap :: (?cmp :: Comparator) => Int -> PQueue -> Stream Elem
unheap n q = unfoldrN n step q where
  step Empty = Nothing
  step (Queue (PHeap x ts)) = Just (x, combine ts)