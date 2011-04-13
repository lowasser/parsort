{-# LANGUAGE ScopedTypeVariables, MultiParamTypeClasses, FlexibleInstances #-}
module Data.Vector.Generic.Mutable.Move (Movable, move, unsafeMove) where

import Control.Monad.Primitive

import Data.Primitive
import Foreign.Storable (Storable)
import Foreign.Marshal.Array

import Data.Vector.Generic.Mutable

import qualified Data.Vector as V
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

instance Movable V.MVector a
instance U.Unbox a => Movable U.MVector a

instance Prim a => Movable P.MVector a where
  basicUnsafeMove (P.MVector dstOff _ dstArr) (P.MVector srcOff srcLen srcArr) =
    memmoveByteArray dstArr (dstOff * sz) srcArr (srcOff * sz) (srcLen * sz)
    where sz = sizeOf (undefined :: a)

instance Storable a => Movable S.MVector a where
  basicUnsafeMove (S.MVector pDst _ _) (S.MVector pSrc lSrc _) = unsafePrimToPrim $ moveArray pDst pSrc lSrc

