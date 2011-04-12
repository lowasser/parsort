{-# LANGUAGE BangPatterns #-}
module Data.Vector.Hacks where

import Control.Monad.Primitive

import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as M
import Data.Vector.Fusion.Stream 
import qualified Data.Vector.Fusion.Stream.Monadic as M
import Data.Vector.Fusion.Stream.Size
import Data.Vector.Fusion.Util

{-# INLINE inline #-}
inline :: G.Vector v a => v a -> v a
inline xs = unsafeUnstream (G.stream xs)

-- | Inlines unsafely all over the place.
{-# INLINE unsafeUnstream #-}
unsafeUnstream :: G.Vector v a => Stream a -> v a
unsafeUnstream (M.Stream step s0 (Exact n)) = unsafeInlineST $ do
  mv <- M.unsafeNew n
  let go !i !s = case unId (step s) of
	  Done	-> return ()
	  Skip s' -> go i s'
	  Yield a !s' -> do
	    M.unsafeWrite mv i a
	    go (i+1) s'
  go 0 s0
  G.unsafeFreeze mv
unsafeUnstream stream = G.unstream stream