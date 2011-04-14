{-# LANGUAGE ImplicitParams, Rank2Types #-}
module Data.Vector.Sort.Parallel.Utils (unsafeSortPermIO, doBoth, RealWorld) where

import Control.Concurrent
import Control.Exception
import Control.Monad.ST

import System.IO.Unsafe

import Data.Vector.Sort.Common

doBoth :: IO () -> IO () -> IO ()
doBoth m1 m2 = do
  lock <- newEmptyMVar
  _ <- forkIO $ m1 `finally` putMVar lock ()
  m2
  takeMVar lock

{-# INLINE unsafeSortPermIO #-}
unsafeSortPermIO :: Vector v a => ((?cmp :: Comparator) => PMVector RealWorld Int -> IO ()) ->
  LEq a -> v a -> v a
unsafeSortPermIO sortImpl (<=?) xs = unsafePerformIO (sortPermIO sortImpl (<=?) xs)