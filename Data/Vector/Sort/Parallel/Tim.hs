{-# LANGUAGE ScopedTypeVariables, RecordWildCards, NamedFieldPuns, BangPatterns, ImplicitParams #-}
{-# OPTIONS -funbox-strict-fields #-}
module Data.Vector.Sort.Parallel.Tim (sort, sortBy) where

import Control.Concurrent
import Control.Monad.Primitive

import Data.Vector.Sort.Common
import qualified Data.Vector.Sort.Tim as Seq

import Data.Vector.Sort.Tim.Run
import Data.Vector.Sort.Tim.Merge
import Data.Vector.Sort.Tim.Constants

import System.IO.Unsafe

import qualified Data.Vector.Sort.Insertion.Binary as BinIns

import Prelude hiding (read, length)

data Run = Run {runOffset :: !Int, runLength :: !Int, lock :: !(MVar ())}

data Stack = Stack :> !Run | End

infixl 6 :>

{-# INLINE sort #-}
sort :: (Vector v a, Ord a) => v a -> v a
sort xs = sortBy (<=) xs

{-# INLINE sortBy #-}
sortBy :: Vector v a => LEq a -> v a -> v a
sortBy (<=?) xs = unsafePerformIO (sortPermIO sortVector (<=?) xs)

sortVector :: (?cmp :: Comparator) => PMVector RealWorld Int -> IO ()
sortVector = parallelSort (Seq.sortVector) sortByM

{-# INLINE sortByM #-}
sortByM :: (?cmp :: Comparator) => PMVector RealWorld Int -> IO ()
sortByM !xs = do
      stack <- consumeRuns End 0
      mergeForceCollapse stack
  where
    n = lengthM xs
    !minRun = minRunLength n
    consumeRuns stack off
      | off >= n	= return stack
      | otherwise	= {-# CORE "consumeRuns" #-} countRunAndMakeAscending (dropM off xs) $ \ runLen -> do
	  let force = min (n - off) minRun
	  lock <- newEmptyMVar
	  forkIO $ do
	    primToPrim $ BinIns.sortByM (sliceM off force xs) runLen
	    putMVar lock ()
	  let runLength = max runLen force
	  let stack' = stack :> Run{runOffset = off, runLength, lock}
	  stack'' <- mergeCollapse stack'
	  consumeRuns stack'' (off + runLength)
    mergeCollapse (stack :> s1 :> s2 :> s3)
      | runLength s1 <= runLength s2 + runLength s3
	  = if runLength s1 < runLength s3 then do
	      s' <- mergeRuns s1 s2
	      mergeCollapse (stack :> s' :> s3)
	    else do
	      s' <- mergeRuns s2 s3
	      mergeCollapse (stack :> s1 :> s')
    mergeCollapse (stack :> s1 :> s2)
      | runLength s1 <= runLength s2
	  = do	s' <- mergeRuns s1 s2
	  	mergeCollapse (stack :> s')
    mergeCollapse stack = return stack
    
    mergeForceCollapse (stack :> s1 :> s2) = do
      s' <- mergeRuns s1 s2
      mergeForceCollapse (stack :> s')
    mergeForceCollapse (End :> Run _ _ lock) = takeMVar lock
    mergeForceCollapse End = return ()

    mergeRuns r1@(Run off1 len1 lock1) r2@(Run _ len2 lock2) = {-# SCC "mergeRuns" #-}
      assert (not (nullRun r1) && not (nullRun r2) && consecutive r1 r2) $ do
	lock <- newEmptyMVar
	forkIO $ do
	  takeMVar lock1
	  takeMVar lock2
	  primToPrim $ merge xs off1 len1 len2 mIN_GALLOP
	  putMVar lock ()
	return (Run off1 (len1 + len2) lock)

consecutive :: Run -> Run -> Bool
consecutive (Run off1 len1 _) (Run off2 _ _) = off2 == off1 + len1

nullRun :: Run -> Bool
nullRun Run{runLength} = runLength == 0