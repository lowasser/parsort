{-# LANGUAGE BangPatterns, ImplicitParams #-}
{-# OPTIONS -funbox-strict-fields #-}
module Data.Vector.Sort.Heap.PHeap (fromStream, stream) where

import Data.Vector.Sort.Common
import Data.Vector.Fusion.Stream.Monadic
import Data.Vector.Fusion.Stream.Size

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

{-# INLINE fromStream #-}
fromStream :: (?cmp :: Comparator, Monad m) => Stream m Elem -> m PQueue
fromStream strm = foldl' insertQ Empty strm where
  insertQ Empty x = Queue (PHeap x Nil)
  insertQ (Queue q) x = Queue (insert q x)

{-# INLINE stream #-}
stream :: (?cmp :: Comparator, Monad m) => Int -> PQueue -> Stream m Elem
stream !n !q = Stream step q (Exact n) where
  step Empty = return Done
  step (Queue (PHeap x ts)) = return (Yield x (combine ts))