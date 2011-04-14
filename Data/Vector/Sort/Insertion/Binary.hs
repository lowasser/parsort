{-# LANGUAGE ImplicitParams #-}
module Data.Vector.Sort.Insertion.Binary where

import Control.Monad
import Control.Monad.Primitive

import Data.Bits

import Data.Vector.Sort.Types
import Data.Vector.Sort.Comparator

import Prelude hiding (length, take, drop, read)

{-# INLINE sort #-}
sort :: (Vector v a, Ord a) => v a -> v a
sort = sortBy (<=)

{-# INLINE sortBy #-}
sortBy :: Vector v a => LEq a -> v a -> v a
sortBy = sortPermM (\ xs -> sortByM xs 1)

{-# INLINE sortByM #-}
sortByM :: (?cmp :: Comparator, PrimMonad m) => PMVector (PrimState m) Int -> Int -> m ()
sortByM xs start = run (max start 1) where
  !n = lengthM xs
  binarySearch key l u cont = bin l u where
    bin l u
      | u <= l	= cont l
      | otherwise = do
	  let k = (u + l) `shiftR` 1
	  x <- read xs k
	  if x <=? key then bin (k+1) u else bin l k
  run start = when (start < n) $ do
    x <- read xs start
    binarySearch x 0 start $ \ k -> do
      let m = start - k
      let moveDst = takeM m (dropM (k+1) xs)
      let moveSrc = takeM m (dropM k xs)
      move moveDst moveSrc
      write xs k x
      run (start+1)