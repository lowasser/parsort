{-# LANGUAGE ScopedTypeVariables, MultiParamTypeClasses, FlexibleInstances, BangPatterns #-}
module Data.Vector.Generic.Mutable.Move (Movable, move, unsafeMove) where

import Control.Monad.Primitive
import Control.Monad.ST

import Data.Primitive
import Foreign.Storable (Storable)
import Foreign.Marshal.Array

import Data.Vector.Generic.Mutable

import qualified Data.Vector.Mutable as V
import qualified Data.Vector.Primitive as P
import qualified Data.Vector.Storable as S
import qualified Data.Vector.Unboxed as U

import Prelude hiding (length)

move :: (Movable v a, PrimMonad m) => v (PrimState m) a -> v (PrimState m) a -> m ()
move dst src
  | length dst == length src	= unsafeMove dst src
  | otherwise			= fail "Length mismatch in move"

unsafeMove :: (Movable v a, PrimMonad m) => v (PrimState m) a -> v (PrimState m) a -> m ()
unsafeMove = basicUnsafeMove

class MVector v a => Movable v a where
  basicUnsafeMove :: PrimMonad m => v (PrimState m) a -> v (PrimState m) a -> m ()
  basicUnsafeMove dst src = do
    src' <- clone src
    unsafeCopy dst src'

instance Movable V.MVector a where
  basicUnsafeMove dst src = primToPrim (moveV dst src)

moveV :: V.MVector s a -> V.MVector s a -> ST s ()
moveV !dst@(V.MVector dOff _ _) !src@(V.MVector sOff sLen _)
    | overlaps dst src	= case compare dOff sOff of
	EQ	-> return ()
	LT	-> P.mapM_ (\ i -> unsafeRead src i >>= unsafeWrite dst i) (P.enumFromN 0 sLen)
	GT	-> do	src' <- clone src
			unsafeCopy dst src'
    | otherwise	= unsafeCopy dst src

instance U.Unbox a => Movable U.MVector a

instance Prim a => Movable P.MVector a where
  {-# SPECIALIZE instance Movable P.MVector Int #-}
  basicUnsafeMove dst@(P.MVector dstOff _ dstArr) src@(P.MVector srcOff srcLen srcArr) = case srcLen of
    0	-> return ()
    1	-> unsafeRead src 0 >>= unsafeWrite dst 0
    2	-> do	x <- unsafeRead src 0
		y <- unsafeRead src 1
		unsafeWrite dst 0 x
		unsafeWrite dst 1 y
    3	-> do	x <- unsafeRead src 0
		y <- unsafeRead src 1
		z <- unsafeRead src 2
		unsafeWrite dst 0 x
		unsafeWrite dst 1 y
		unsafeWrite dst 2 z
    _ -> memmoveByteArray dstArr (dstOff * sz) srcArr (srcOff * sz) (srcLen * sz)
    where sz = sizeOf (undefined :: a)

instance Storable a => Movable S.MVector a where
  basicUnsafeMove (S.MVector pDst _ _) (S.MVector pSrc lSrc _) = unsafePrimToPrim $ moveArray pDst pSrc lSrc

