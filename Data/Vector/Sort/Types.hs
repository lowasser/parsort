{-# LANGUAGE CPP, BangPatterns, FlexibleContexts #-}
module Data.Vector.Sort.Types 
  (LEq, VVector, VMVector, PVector, PMVector, Mov.Movable,
  PrimMonad, PrimState, Elem,
  Vector, MVector, Mutable, G.modify, G.length, lengthM,
  index, indexM, read, write, take, drop, slice, takeM, dropM, sliceM, swap,
  copy, copyM, move, moveBy, backpermute, ST,
  goodIndex, goodRange, assert
  ) where

import Control.Monad.Primitive
import Control.Monad.ST
import Control.Exception.Base

import Data.Vector.Generic (Vector, Mutable, length)
import qualified Data.Vector.Generic as G
import Data.Vector.Generic.Mutable (MVector)
import qualified Data.Vector.Fusion.Stream as Stream
import qualified Data.Vector.Generic.Mutable.Move as Mov
import qualified Data.Vector.Generic.Mutable as M
import qualified Data.Vector as V
import qualified Data.Vector.Primitive as P

import Prelude hiding (length, take, drop, read)

type LEq a = a -> a -> Bool
type VVector = V.Vector
type VMVector = V.MVector
type PVector = P.Vector
type PMVector = P.MVector
type Elem = Int

#include "asserts.h"

lengthM :: MVector v a => v s a -> Int
lengthM = M.length

index :: Vector v a => v a -> Int -> a
indexM :: (Monad m, Vector v a) => v a -> Int -> m a

read :: (PrimMonad m, MVector v a) => v (PrimState m) a -> Int -> m a
write :: (PrimMonad m, MVector v a) => v (PrimState m) a -> Int -> a -> m ()
swap :: (PrimMonad m, MVector v a) => v (PrimState m) a -> Int -> Int -> m ()

take, drop :: Vector v a => Int -> v a -> v a
slice :: Vector v a => Int -> Int -> v a -> v a
takeM, dropM :: MVector v a => Int -> v s a -> v s a
sliceM :: MVector v a => Int -> Int -> v s a -> v s a

copy :: (PrimMonad m, Vector v a) => G.Mutable v (PrimState m) a -> v a -> m ()
copyM :: (PrimMonad m, MVector v a) => v (PrimState m) a -> v (PrimState m) a -> m ()

move :: (PrimMonad m, Mov.Movable v a) => v (PrimState m) a -> v (PrimState m) a -> m ()

{-# INLINE backpermute #-}
backpermute :: (Vector v a, Vector v' Int) => v a -> v' Int -> v a
backpermute !v is = G.unstream $ Stream.unbox $ Stream.map (indexM v) $ G.stream is

#ifdef DEBUG
index xs i = (G.!) xs i
indexM = G.indexM
read = M.read
write = M.write
take k xs = G.take k xs
drop = G.drop
slice = G.slice
takeM = M.take
dropM = M.drop
sliceM = M.slice
copy = G.copy
copyM = M.copy
move = Mov.move
swap = M.swap
#else
index = G.unsafeIndex
indexM = G.unsafeIndexM
read = M.unsafeRead
write = M.unsafeWrite
asserter _ _ b = b
take = G.unsafeTake
drop = G.unsafeDrop
slice = G.unsafeSlice
takeM = M.unsafeTake
dropM = M.unsafeDrop
sliceM = M.unsafeSlice
copy = G.unsafeCopy
copyM = M.unsafeCopy
move = Mov.unsafeMove
swap = M.unsafeSwap
#endif

{-# INLINE moveBy #-}
moveBy :: (Mov.Movable v a, PrimMonad m) => v (PrimState m) a -> Int -> Int -> Int -> m ()
moveBy mv off len dist =
  move (takeM len (dropM (off + dist) mv))
	(takeM len (dropM off mv))

goodIndex :: Vector v a => Int -> v a -> Bool
goodIndex i xs = i >= 0 && i < length xs

goodRange :: Vector v a => Int -> Int -> v a -> Bool
goodRange i j xs  = i >= 0 && i <= j && j <= length xs