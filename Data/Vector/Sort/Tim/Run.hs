{-# LANGUAGE BangPatterns, DoAndIfThenElse, RecordWildCards, NamedFieldPuns, CPP, ScopedTypeVariables, FlexibleContexts #-}
{-# OPTIONS -funbox-strict-fields #-}
module Data.Vector.Sort.Tim.Run (countRunAndMakeAscending, mergeLo, mergeLoV) where

import Control.Monad.Primitive

import Data.Vector.Generic (freeze, stream, unsafeFreeze)
import Data.Vector.Generic.Mutable (reverse)

import Data.Vector.Sort.Types

import Data.Vector.Sort.Tim.Hop
import Data.Vector.Sort.Tim.Constants

import Foreign.Marshal.Alloc
import Foreign.Storable
import Foreign.Ptr

import Prelude hiding (length, read, reverse, mapM_, take, drop)

{-# SPECIALIZE countRunAndMakeAscending ::
      PrimMonad m => LEq Int -> PMVector (PrimState m) Int -> (Int -> m b) -> m b,
      PrimMonad m => LEq a -> VMVector (PrimState m) a -> (Int -> m b) -> m b #-}
countRunAndMakeAscending :: (PrimMonad m, MVector v a) => 
  LEq a -> v (PrimState m) a -> (Int -> m b) -> m b
countRunAndMakeAscending (<=?) xs cont
  | n <= 1	= cont n
  | otherwise	= do
      x0 <- read xs 0
      x1 <- read xs 1
      if x0 <=? x1 then countAscendingRun x1 1 else countDescendingRun x1 1
  where	n = lengthM xs
	descCont k = do
	  reverse (takeM k xs)
	  cont k
	countAscendingRun xi !i
	  | i' < n	= do
	      xi' <- read xs i'
	      if xi <=? xi' then countAscendingRun xi' i' else cont i'
	  | otherwise	= cont n
	  where i' = i + 1
	countDescendingRun xi !i
	  | i' < n	= do
	      xi' <- read xs i'
	      if xi <=? xi' then descCont i' else countAscendingRun xi' i'
	  | otherwise	= descCont n
	  where i' = i + 1

data MergeStatus = Status
  {count1, count2, cursor1, cursor2, dest :: !Int}

#define INCR(field,record,amt) ((\ x -> x{field = field x + amt}) (record))

modifyPtr :: Storable a => (a -> a) -> Ptr a -> IO ()
modifyPtr f ptr = do
  a <- peek ptr
  poke ptr (f a)

decr :: Ptr Int -> IO ()
decr = modifyPtr (subtract 1)

mergeLoV :: forall a . Int -> LEq a -> VMVector RealWorld a -> Int -> Int -> Int -> IO Int
mergeLoV mG (<=?) xs off1 len1 len2 =
  mergeLo (undefined :: VVector a) mG (<=?) xs off1 len1 len2

{-# INLINE mergeLo #-}
-- Assumes the first element of run 1 is greater than the first element of run 2, and
-- the last element of run 1 is greater than all elements of run 2.
mergeLo :: forall v a . (Vector v a, Movable (Mutable v) a)
    => v a -> Int -> LEq a -> Mutable v RealWorld a -> Int -> Int -> Int -> IO Int
mergeLo _ !minGallop (<=?) xs !off1 len1 len2 
  | checks $ len2 == 1 = do
      x <- read run2 0
      move (takeM len1 destArr) (dropM 1 destArr)
      write destArr 0 x
      return minGallop
  | len1 == 1	= do
      x <- read xs off1
      move (takeM len2 destArr) run2
      write destArr len2 x
      return minGallop
  | otherwise	= alloca $ \ gallopPtr -> do
      poke gallopPtr minGallop
      run1 <- freeze (takeM len1 (dropM off1 xs))
      let curRun1 :: MergeStatus -> v a
	  curRun1 s@Status{..} = drop cursor1 run1
	  curRun2 :: MergeStatus -> Mutable v RealWorld a
	  curRun2 s@Status{..} = dropM cursor2 run2
	  curDest s@Status{..} = dropM dest destArr
	  curs1 s@Status{..} = return (index (curRun1 s) 0)
	  curs2 s@Status{..} = read (curRun2 s) 0
	  {-# INLINE advance1 #-}
	  advance1 !s !k cont = do
	    copy (takeM k (curDest s)) (take k (curRun1 s))
	    let s' = 
		  INCR(dest, INCR(cursor1, INCR(count1, s{count2 = 0}, k), k), k)
	    if breakNow s' then done s' else cont s'
	  {-# INLINE advance2 #-}
	  advance2 !s !k cont = do
	    copyM (takeM k (curDest s)) (takeM k (curRun2 s))
	    let s' = 
		  INCR(dest, INCR(cursor2, INCR(count2, s{count1 = 0}, k), k), k)
	    if breakNow s' then done s' else cont s'
	  breakNow Status{..} = cursor1 < len1 - 1 && cursor2 < len2 - 1
	  gallopNow s@Status{..} = do
	    curGallop <- peek gallopPtr
	    return (count1 + count2 >= curGallop)
	  step s@Status{..} = assert (not (breakNow s)) $ do
	    shouldGallop <- gallopNow s
	    if shouldGallop then doGallop1 s else do
	      x1 <- curs1 s
	      x2 <- curs2 s
	      if x1 <=? x2 then advance1 s 1 step
		else advance2 s 1 step
	  doGallop1 s = assert (not (breakNow s)) $ do
	    key2 <- curs2 s
	    let !count1' = gallopRight (<=?) key2 (curRun1 s) 0
	    advance1 s{count1=0} count1' $ \ s' -> advance2 s' 1 doGallop2
	  doGallop2 s = do
	    key1 <- curs1 s
	    run2' <- unsafeFreeze (curRun2 s)
	    let !count2' = gallopLeft (<=?) key1 (run2' :: v a) 0
	    advance2 s{count2=0} count2' $ \ s' -> advance1 s' 1 $ \ s''@Status{..} -> do
	      decr gallopPtr
	      if count1 >= mIN_GALLOP || count2 >= mIN_GALLOP
		then doGallop1 s''
		else do
		  modifyPtr (\ g -> max 2 (g + 2)) gallopPtr
		  step s''
	  done :: MergeStatus -> IO ()
	  done s@Status{..} = case (len1 - cursor1, len2 - cursor2) of
	      (1, l2) -> assert (l2 > 0) $ do
		move (takeM l2 $ curDest s) (curRun2 s)
		write xs (off1 + len1 + len2 - 1) (index (curRun1 s) 0)
	      (l1, 0) -> assert (l1 > 1) $ copy (curDest s) (curRun1 s)
	      _ -> fail "Method invariant violated!"
      write destArr 0 =<< read run2 0
      step Status{count1 = 0, count2 = 0, cursor1 = 0, cursor2 = 1, dest = 1}
      g <- peek gallopPtr
      return (max g 1)
  where	!off2 = off1 + len1
	checks = assert (len1 <= len2) . checkRangeM off1 (off1 + len1) xs . checkRangeM off2 (off2 + len2) xs
	!run2 = takeM len2 (dropM off2 xs)
	!destArr = takeM (len1 + len2) (dropM off1 xs)