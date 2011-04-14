{-# LANGUAGE CPP, BangPatterns, FlexibleContexts #-}
module Data.Vector.Sort.Types 
  (LEq, VVector, VMVector, PVector, PMVector, Mov.Movable,
  PrimMonad, PrimState,
  Vector, MVector, Mutable, G.modify, G.length, lengthM,
  index, indexM, read, write, take, drop, takeM, dropM, swap,
  copy, copyM, move, backpermute,
  checkIndex, checkRange, checkRangeM, assert, assertM
  ) where

import Control.Monad.Primitive
import Control.Exception.Base

import Text.Printf

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

lengthM :: MVector v a => v s a -> Int
lengthM = M.length

index :: Vector v a => v a -> Int -> a
indexM :: (Monad m, Vector v a) => v a -> Int -> m a

read :: (PrimMonad m, MVector v a) => v (PrimState m) a -> Int -> m a
write :: (PrimMonad m, MVector v a) => v (PrimState m) a -> Int -> a -> m ()
swap :: (PrimMonad m, MVector v a) => v (PrimState m) a -> Int -> Int -> m ()

take, drop :: Vector v a => Int -> v a -> v a
takeM, dropM :: MVector v a => Int -> v s a -> v s a

copy :: (PrimMonad m, Vector v a) => G.Mutable v (PrimState m) a -> v a -> m ()
copyM :: (PrimMonad m, MVector v a) => v (PrimState m) a -> v (PrimState m) a -> m ()

move :: (PrimMonad m, Mov.Movable v a) => v (PrimState m) a -> v (PrimState m) a -> m ()

{-# INLINE backpermute #-}
backpermute :: (Vector v a, Vector v' Int) => v a -> v' Int -> v a
backpermute !v is = G.unstream $ Stream.unbox $ Stream.map (indexM v) $ G.stream is

asserter :: Bool -> String -> b -> b

#ifdef DEBUG
index xs i = checkIndex i xs ((G.!) xs i)
indexM = G.indexM
read = M.read
write = M.write
asserter True _ b = b
asserter False x _ = error x
take k xs = checkRange 0 k xs $ G.take k xs
drop = G.drop
takeM = M.take
dropM = M.drop
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
takeM = M.unsafeTake
dropM = M.unsafeDrop
copy = G.unsafeCopy
copyM = M.unsafeCopy
move = Mov.unsafeMove
swap = M.unsafeSwap
#endif

{-# INLINE checkIndex #-}
checkIndex :: Vector v a => Int -> v a -> b -> b
checkIndex i xs z = 
  asserter (0 <= i && i < length xs)
    (printf "Index %d out of bounds %d" i (length xs))
    z

{-# INLINE checkRangeM #-}
checkRangeM :: MVector v a => Int -> Int -> v s a -> b -> b
checkRangeM i j xs z = asserter (0 <= i && i <= j && j <= lengthM xs)
    (printf "Range [%d, %d) out of bounds %d" i j (lengthM xs))
    z

{-# INLINE checkRange #-}
checkRange :: Vector v a => Int -> Int -> v a -> b -> b
checkRange i j xs z = asserter (0 <= i && i <= j && j <= length xs)
    (printf "Range [%d, %d) out of bounds %d" i j (length xs))
    z

assertM :: Monad m => Bool -> m ()
assertM b = assert b (return ())