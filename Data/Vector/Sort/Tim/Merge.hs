{-# LANGUAGE BangPatterns, CPP, ImplicitParams, ScopedTypeVariables, DoAndIfThenElse, NamedFieldPuns #-}
{-# OPTIONS -funbox-strict-fields #-}
module Data.Vector.Sort.Tim.Merge (merge) where

import Control.Monad
import Control.Monad.State.Class

import Data.Vector.Sort.Common
import Data.Vector.Primitive (unsafeFreeze)
import Data.Vector.Generic.Mutable (clone)
import Data.Vector.Sort.Tim.Monad
import Data.Vector.Sort.Tim.Hop
import Data.Vector.Sort.Tim.Constants

import Prelude hiding (read, take, drop)

#include "asserts.h"

decr :: Int -> Int
decr = subtract 1

startGallop, gallopMore :: MergeM s Bool
startGallop =  do
  c1 <- gets (count . status1)
  c2 <- gets (count . status2)
  g <- gets gallop
  return (c1 + c2 >= g)

gallopMore = do
  c1 <- gets (count . status1)
  c2 <- gets (count . status2)
  return (c1 >= mIN_GALLOP || c2 >= mIN_GALLOP)

ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM cond thenM elseM = do
  b <- cond
  if b then thenM else elseM

merge :: (?cmp :: Comparator) =>  PMVector s Int -> Int -> Int -> Int -> Int -> ST s Int
mergeLo, mergeHi :: forall s . (?cmp :: Comparator) => PMVector s Int -> Int -> Int -> ST s Int
merge !arr !off1 !len1 !len2 !minGallop = do
  run1 <- unsafeFreeze (sliceM off1 len1 arr)
  run2 <- unsafeFreeze (sliceM off2 len2 arr)
  first2 <- indexM run2 0
  let !k = gallopRight first2 run1 0
  ASSERTM(k >= 0)
  let off1' = off1 + k
      len1' = len1 - k
      run1' = sliceM off1' len1' arr
  if len1' == 0 then return minGallop else do
    last1 <- read run1' (len1' - 1)
    let !len2' = gallopLeft last1 run2 (len2 - 1)
    if len2' == 0 then return minGallop else do
      let arr' = sliceM off1' (len1' + len2') arr
      let off2' = len1'
      if len1' <= len2' then mergeLo arr' off2' minGallop
	else mergeHi arr' off2' minGallop
  where off2 = off1 + len1

mergeLo !arr !off2 !minGallop
  | length2 == 1 = do
      x <- read arr off2
      moveBy arr 0 length1 1
      write arr 0 x
      return minGallop
  | length1 == 1 = do
      x <- read arr 0
      moveBy arr off2 length2  (-1)
      write arr length2 x
      return minGallop
  | otherwise	= do
      run1 <- clone (takeM length1 arr)
      let !run2 = dropM off2 arr
      let get1 = do
	    c1 <- gets (cursor . status1)
	    liftST (read run1 c1)
	  get2 = do
	    c2 <- gets (cursor . status2)
	    liftST (read run2 c2)
	  writeDest x = do
	    d <- updateDest (+1)
	    liftST (write arr d x)
	  inBounds1 = do
	    l1 <- gets (len . status1)
	    return (l1 > 1)
	  inBounds2 = do
	    l2 <- gets (len . status2)
	    return (l2 > 0)
	  break1 m = ifM inBounds1 m done
	  break2 m = ifM inBounds2 m done
	  assertInBounds = do
	    inB1 <- inBounds1
	    inB2 <- inBounds2
	    ASSERTM(inB1 && inB2)
	  step = do
	    assertInBounds
	    x1 <- get1
	    x2 <- get2
	    if x1 <=? x2 then do
	      update1 (incrStatus 1)
	      update2 resetStatus
	      writeDest x1
	      break1 stepLoop
	    else do
	      update2 (incrStatus 1)
	      update1 resetStatus
	      writeDest x2
	      break2 stepLoop
	  stepLoop = ifM startGallop doGallop step
	  curRun1 = do
	    Status{cursor, len} <- gets status1
	    liftST $ unsafeFreeze (sliceM cursor len run1)
	  curRun2 = do
	    Status{cursor, len} <- gets status2
	    liftST $ unsafeFreeze (sliceM cursor len run2)
	  doGallop = doGallop1
	  doGallop1 = do
	    assertInBounds
	    theRun1 <- curRun1
	    key2 <- get2
	    let !jump = gallopRight key2 theRun1 0
	    if jump /= 0 then do
	      d <- updateDest (+jump)
	      liftST $ copy (sliceM d jump arr) (take jump theRun1)
	      update1 (incrStatus jump . resetStatus)
	      break1 (endGallop1 key2)
	    else endGallop1 key2
	  endGallop1 !k2 = do
	    writeDest k2
	    update2 (incrStatus 1)
	    break2 doGallop2
	  doGallop2 = do
	    assertInBounds
	    c2 <- gets (cursor . status2)
	    theRun2 <- curRun2
	    key1 <- get1
	    let !jump = gallopLeft key1 theRun2 0
	    if jump /= 0 then do
	      d <- updateDest (+jump)
	      liftST $ move (sliceM d jump arr) (sliceM c2 jump run2)
	      update2 (incrStatus jump . resetStatus)
	      break2 (endGallop2 key1)
	    else endGallop2 key1
	  endGallop2 !k1 = do
	    writeDest k1
	    update1 (incrStatus 1)
	    break1 $ do
	      updateGallop decr
	      ifM gallopMore doGallop (updateGallop (\ g -> max (g+2) 2) >> step)
	  done = do
	    Status{len=l1, cursor = c1} <- gets status1
	    Status{len=l2, cursor = c2} <- gets status2
	    d <- gets dest
	    liftST $ move (sliceM d l2 arr) (sliceM c2 l2 run2)
	    liftST $ copyM (sliceM (d+l2) l1 arr) (sliceM c1 l1 run1)
	  s0 = MergeStatus {
		  status1 = Status{cursor = 0, len = length1, count = 0},
		  status2 = Status{cursor = 0, len = length2, count = 0},
		  dest = 0,
		  gallop = minGallop}
      execMergeM s0 $ do
	x0 <- get2
	update2 (incrStatus 1)
	writeDest x0
	step
  where n = lengthM arr
	length1 = off2
	length2 = n - off2

mergeHi !arr !off2 !minGallop
  | length2 == 1 = do
      x <- read arr off2
      moveBy arr 0 length1 1
      write arr 0 x
      return minGallop
  | length1 == 1 = do
      x <- read arr 0
      moveBy arr off2 length2 (-1)
      write arr length2 x
      return minGallop
  | otherwise	= do
      let run1 = takeM off2 arr
      run2 <- clone (dropM off2 arr)
      let get1 = do
	    c1 <- gets (cursor . status1)
	    liftST (read run1 c1)
	  get2 = do
	    c2 <- gets (cursor . status2)
	    liftST (read run2 c2)
	  writeDest x = do
	    d <- updateDest decr
	    liftST (write arr d x)
	  inBounds1 = do
	    l1 <- gets (len . status1)
	    return (l1 > 0)
	  inBounds2 = do
	    l2 <- gets (len . status2)
	    return (l2 > 1)
	  assertInBounds = do
	    inB1 <- inBounds1
	    inB2 <- inBounds2
	    ASSERTM(inB1 && inB2)
	  break1 m = ifM inBounds1 m done
	  break2 m = ifM inBounds2 m done
	  step = do
	    assertInBounds
	    x1 <- get1
	    x2 <- get2
	    if x1 <=? x2 then do
	      writeDest x2
	      update1 resetStatus
	      update2 (decrStatus 1)
	      break2 stepLoop
	    else do
	      writeDest x1
	      update2 resetStatus
	      update1 (decrStatus 1)
	      break1 stepLoop
	  stepLoop = ifM startGallop doGallop step
	  doGallop = doGallop1
	  curRun1 = do
	    l1 <- gets (len . status1)
	    liftST $ unsafeFreeze (takeM l1 run1)
	  curRun2 = do
	    l2 <- gets (len . status2)
	    liftST $ unsafeFreeze (takeM l2 run2)
	  doGallop1 = do
	    assertInBounds
	    key2 <- get2
	    l1 <- gets (len . status1)
	    theRun1 <- curRun1
	    let !jump = l1 - gallopRight key2 theRun1 (l1 - 1)
	    if jump /= 0 then do
	      updateDest (subtract jump)
	      d <- gets dest
	      update1 (decrStatus jump . resetStatus)
	      c1 <- gets (cursor . status1)
	      liftST $ move (sliceM (d+1) jump arr) (sliceM (c1+1) jump run1)
	      break1 (endGallop1 key2)
	    else endGallop1 key2
	  endGallop1 key2 = do
	    writeDest key2
	    update2 (decrStatus 1)
	    break2 doGallop2
	  doGallop2 = do
	    assertInBounds
	    key1 <- get1
	    l2 <- gets (len . status2)
	    theRun2 <- curRun2
	    let !jump = l2 - gallopLeft key1 theRun2 (l2 - 1)
	    if jump /= 0 then do
	      updateDest (subtract jump)
	      d <- gets dest
	      update2 (decrStatus jump . resetStatus)
	      c2 <- gets (cursor . status2)
	      liftST $ copyM (sliceM (d+1) jump arr) (sliceM (c2 + 1) jump run2)
	      break2 (endGallop2 key1)
	    else endGallop2 key1
	  endGallop2 key1 = do
	    writeDest key1
	    update1 (decrStatus 1)
	    break1 $ do
	      updateGallop decr
	      ifM gallopMore doGallop $ updateGallop (\ g -> max (g+2) 2) >> step
	  done = do
	    Status{len=l1, cursor = c1} <- gets status1
	    Status{len=l2, cursor = c2} <- gets status2
	    d <- gets dest
	    liftST $ move (sliceM (d - l1 + 1) l1 arr) (sliceM (c1 - l1 + 1) l1 run1)
	    liftST $ copyM (sliceM (d - l1 - l2 + 1) l2 arr) (sliceM (c2 - l2 + 1) l2 run2)
	  s0 = MergeStatus {
		  status1 = Status{cursor = length1-1, len = length1, count = 0},
		  status2 = Status{cursor = length2-1, len = length2, count = 0},
		  dest = n - 1,
		  gallop = minGallop}
      execMergeM s0 $ do
	x0 <- get1
	update1 (decrStatus 1)
	writeDest x0
	step
  where n = lengthM arr
	length1 = off2
	length2 = n - off2