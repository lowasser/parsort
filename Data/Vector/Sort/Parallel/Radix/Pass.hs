{-# LANGUAGE ScopedTypeVariables, BangPatterns #-}
module Data.Vector.Sort.Parallel.Radix.Pass (radixPass) where

import GHC.Conc
import Control.Concurrent
import Control.Monad (replicateM_)

import Data.Vector.Sort.Types
import Data.Vector.Sort.Radix.Utils
import Data.Vector.Sort.Parallel.Stream

import Data.Vector.Fusion.Stream (liftStream)
import Data.Vector.Fusion.Stream.Monadic (zipWith, prescanl)
import Data.Vector.Generic (stream)
import Data.Vector.Generic.Mutable (unsafeNew, transform)

import qualified Data.Vector as V
import qualified Data.Vector.Primitive as P

import Data.Word

import Prelude hiding (read, zipWith, length)

{-# INLINE radixPass #-}
radixPass :: P.Prim a => (a -> Word8) -> PVector a -> IO (PVector a)
radixPass look !arr = do
  let !n = length arr
      !nCaps = numCapabilities
      !chunkSize = (n + nCaps - 1) `quot` nCaps
      chunkAt i = let k = chunkSize `min` n - i in slice i k arr
      chunkCount i = P.create $ countUp $ P.map look $ chunkAt i
      !chunkCounts = parVector (V.map chunkCount $ V.enumFromStepN 0 chunkSize nCaps)
  tmp <- unsafeNew n
  lock <- newEmptyMVar
  let go_move (i, counter0) = forkOnIO i $ do
	!counter <- P.thaw counter0
	let do_move x = do
	      let !b = fromIntegral $ look x
	      j <- read counter b
	      write counter b (j+1)
	      write tmp j x
	P.mapM_ do_move (chunkAt (i * chunkSize))
	putMVar lock ()
  V.mapM_ go_move $ V.imap (,) $ offsets chunkCounts
  replicateM_ nCaps (takeMVar lock)
  P.unsafeFreeze tmp

{-# INLINE offsets #-}
offsets :: VVector (PVector Int) -> VVector (PVector Int)
offsets !chunkCounts = V.prescanl (P.zipWith (+)) tots chunkCounts
  where !tots = totalOffsets chunkCounts

totalOffsets :: VVector (PVector Int) -> PVector Int
totalOffsets !counts = P.create $ do
  tot <- unsafeNew 256
  zeroOut tot
  V.forM_ counts $ \ count -> transform (zipWith (+) (liftStream $ stream count)) tot
  transform (prescanl (+) 0) tot
  return tot
