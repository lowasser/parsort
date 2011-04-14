{-# LANGUAGE ScopedTypeVariables, RecordWildCards, NamedFieldPuns, BangPatterns, ImplicitParams #-}
{-# OPTIONS -funbox-strict-fields #-}
module Data.Vector.Sort.Tim (sort, sortBy) where

import Control.Monad.Primitive

import Data.Vector.Generic (unsafeFreeze)

import Foreign.Storable
import Foreign.Ptr
import Foreign.Marshal.Alloc

import Data.Vector.Sort.Types
import Data.Vector.Sort.Comparator

import Data.Vector.Sort.Tim.Run
import Data.Vector.Sort.Tim.Hop
import Data.Vector.Sort.Tim.Constants

import System.IO.Unsafe

import qualified Data.Vector.Sort.Insertion.Binary as BinIns

import Prelude hiding (read, length)

data Run = Run {runOffset :: !Int, runLength :: !Int} deriving (Show)

data Stack = Stack :> !Run | End deriving (Show)

infixl 6 :>

{-# INLINE sort #-}
sort :: (Vector v a, Ord a) => v a -> v a
sort xs = sortBy (<=) xs

{-# INLINE sortBy #-}
sortBy :: Vector v a => LEq a -> v a -> v a
sortBy (<=?) xs = unsafePerformIO (sortPermIO sortVector (<=?) xs)

sortVector :: (?cmp :: Comparator) => PMVector RealWorld Int -> IO ()
sortVector !mv = alloca $ \ gallopPtr -> do
	poke gallopPtr mIN_GALLOP
	sortByM gallopPtr mv

{-# INLINE sortByM #-}
sortByM :: (?cmp :: Comparator) => Ptr Int -> PMVector RealWorld Int -> IO ()
sortByM !gallopPtr !xs
  | n < 2		= return ()
  | n < mIN_MERGE	= countRunAndMakeAscending xs $ primToPrim . BinIns.sortByM xs
  | otherwise		= do
      stack <- consumeRuns End 0
      mergeForceCollapse stack
  where
    n = lengthM xs
    !minRun = minRunLength n
    consumeRuns stack off
      | off >= n	= return stack
      | otherwise	= {-# CORE "consumeRuns" #-} countRunAndMakeAscending (dropM off xs) $ \ runLen -> do
	  let force = min (n - off) minRun
	  primToPrim $ BinIns.sortByM (takeM force (dropM off xs)) runLen
	  let runLength = max runLen force
	  let stack' = stack :> Run{runOffset = off, runLength}
	  stack'' <- mergeCollapse stack'
	  consumeRuns stack'' (off + runLength)
    mergeCollapse (stack :> s1 :> s2 :> s3)
      | runLength s1 <= runLength s2 + runLength s3
	  = if runLength s1 < runLength s3 then do
	      mergeRuns s1 s2
	      mergeCollapse (stack :> concatRun s1 s2 :> s3)
	    else do
	      mergeRuns s2 s3
	      mergeCollapse (stack :> s1 :> concatRun s2 s3)
    mergeCollapse (stack :> s1 :> s2)
      | runLength s1 <= runLength s2
	  = do	mergeRuns s1 s2
	  	mergeCollapse (stack :> concatRun s1 s2)
    mergeCollapse stack = return stack
    
    mergeForceCollapse (stack :> s1 :> s2) = do
      mergeRuns s1 s2
      mergeForceCollapse (stack :> concatRun s1 s2)
    mergeForceCollapse _ = return ()

    mergeRuns r1 r2 = {-# CORE "mergeRuns" #-} assert (not (nullRun r1) && not (nullRun r2) && consecutive r1 r2) $ do
      let run1 = sliceRun r1 xs
	  run2 = sliceRun r2 xs
      first2 <- read run2 0
      k <- gallopRightM first2 run1 0
      assertM (k >= 0)
      let r1' = dropRun k r1
      let run1' = sliceRun r1' xs
      if nullRun r1' then return () else do
	last1 <- read run1' (runLength r1' - 1)
	len2' <- gallopLeftM last1 run2 (lengthM run2 - 1)
	assertM (len2' >= 0)
	if len2' == 0 then return () else do
	  g <- peek gallopPtr
	  g' <- merge g xs (runOffset r1') (runLength r1') len2'
	  poke gallopPtr g'

consecutive :: Run -> Run -> Bool
consecutive (Run off1 len1) (Run off2 _) = off2 == off1 + len1

concatRun :: Run -> Run -> Run
concatRun (Run off1 len1) (Run _ len2) = Run off1 (len1 + len2)

nullRun :: Run -> Bool
nullRun Run{runLength} = runLength == 0

dropRun :: Int -> Run -> Run
dropRun k Run{..} = Run{runOffset = runOffset + k, runLength = runLength - k}

sliceRun :: MVector v a => Run -> v s a -> v s a
sliceRun Run{..} xs = takeM runLength (dropM runOffset xs)

gallopLeftM, gallopRightM :: (?cmp :: Comparator, PrimMonad m) =>
  Int -> PMVector (PrimState m) Int -> Int -> m Int
gallopLeftM key mv hint = do
  v <- unsafeFreeze mv
  return (gallopLeft key (v :: PVector Int) hint)
gallopRightM key mv hint = do
  v <- unsafeFreeze mv
  return (gallopRight key (v :: PVector Int) hint)