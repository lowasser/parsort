{-# LANGUAGE BangPatterns, ImplicitParams #-}
module Data.Vector.Sort.Parallel.Quick where

import Control.Monad.Primitive
import Control.Concurrent
import Control.Exception
import System.IO.Unsafe

import Data.Vector.Sort.Types
import Data.Vector.Sort.Comparator
import Data.Vector.Sort.Quick.Pivot
import Data.Vector.Sort.Quick.Partition
import qualified Data.Vector.Sort.Insertion as Ins

import Prelude hiding (length, read)

{-# INLINE sort #-}
sort :: (Vector v a, Ord a) => v a -> v a
sort = sortBy (<=)

{-# INLINE sortBy #-}
sortBy :: Vector v a => LEq a -> v a -> v a
sortBy (<=?) xs = unsafePerformIO $ sortPermIO sortByM (<=?) xs

doBoth :: IO () -> IO () -> IO ()
doBoth m1 m2 = do
  lock <- newEmptyMVar
  _ <- forkIO $ m1 `finally` putMVar lock ()
  m2
  takeMVar lock

{-# INLINE sortByM #-}
sortByM :: (?cmp :: Comparator) => PMVector RealWorld Int -> IO ()
sortByM = let
  qSort xs
    | n <= 20	= primToPrim $ Ins.sortByM xs
    | otherwise	= pickPivot xs $ \ pivotIndex ->
	partition pivotIndex xs $ \ breakIndex ->
	    let doLeft = qSort (takeM breakIndex xs)
		doRight = qSort (dropM (breakIndex + 1) xs)
	    in doBoth doLeft doRight
    where n = lengthM xs
  in qSort