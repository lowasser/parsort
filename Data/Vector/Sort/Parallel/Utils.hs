module Data.Vector.Sort.Parallel.Utils (unsafePerformIO, doBoth) where

import Control.Concurrent
import Control.Exception
import System.IO.Unsafe

doBoth :: IO () -> IO () -> IO ()
doBoth m1 m2 = do
  lock <- newEmptyMVar
  forkIO $ m1 `finally` putMVar lock ()
  m2
  takeMVar lock