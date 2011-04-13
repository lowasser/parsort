{-# LANGUAGE FlexibleContexts #-}
module Data.Vector.Sort.Parallel.Quick (sort, sortBy) where

import Control.Parallel
import Control.Exception.Base
import Control.Monad.Primitive

import Data.Bits

import qualified Data.Vector.Generic as G
import Data.Vector.Generic.Mutable
import Data.Vector.Generic.Mutable.Move

import qualified Data.Vector.Sort.Quick as Q

import qualified Data.Vector as V
import qualified Data.Vector.Primitive as P

import Prelude hiding (length)

{-# SPECIALIZE sort ::
      P.Vector Int -> P.Vector Int,
      Ord a => V.Vector a -> V.Vector a #-}
sort :: (G.Vector v a, Movable (G.Mutable v) a, Ord a) => v a -> v a
sort xs = sortBy (<=) xs

{-# INLINE sortBy #-}
sortBy :: (G.Vector v a, Movable (G.Mutable v) a) => (a -> a -> Bool) -> v a -> v a
sortBy (<=?) xs = unsafeInlineIO $ do
  let !n = G.length xs
  mv <- unsafeNew n
  G.unsafeCopy mv xs
  quickSortM (<=?) mv
  G.unsafeFreeze mv

{-# INLINE quickSortM #-}
quickSortM :: Movable v a => (a -> a -> Bool) -> v (PrimState IO) a -> IO ()
quickSortM (<=?) = let
  qSort xs
    | n <= 1000	= Q.sortByM (<=?) xs
    | otherwise	= do
	a <- unsafeRead xs 0
	b <- unsafeRead xs (n `shiftR` 1)
	c <- unsafeRead xs (n - 1)
	let !pivot = medOf3 (<=?) a b c
	pivotIndex <- unstablePartition (<=? pivot) xs
	let finL = unsafeInlineIO (qSort $ unsafeTake pivotIndex xs)
	finL `par` qSort (unsafeDrop pivotIndex xs)
	evaluate finL
    where n = length xs
  in qSort

medOf3 :: (a -> a -> Bool) -> a -> a -> a -> a
medOf3 (<=) a b c = case (a <= b, b <= c, a <= c) of
  (True, True, _)	-> b
  (True, False, True)	-> c
  (True, False, False)	-> a
  (False, _, True)	-> a
  (False, True, False)	-> c
  (False, False, _)	-> b