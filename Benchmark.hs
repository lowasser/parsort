module Benchmark where

import Control.Monad

import Criterion.Main
import System.Random.MWC

import Data.Vector.Primitive (Vector, convert)

import qualified Data.Vector.Sort.Insertion as Ins
import qualified Data.Vector.Sort.Insertion.Binary as BI
import qualified Data.Vector.Sort.Merge as Merge
import qualified Data.Vector.Sort.Parallel.Merge as MergePar

insertion :: Vector Int -> Benchmark
insertion xs = bench "Insertion sort" (whnf Ins.sort xs)

binaryInsertion :: Vector Int -> Benchmark
binaryInsertion xs = bench "Binary insertion sort" (whnf BI.sort xs)

merge :: Vector Int -> Benchmark
merge xs = bench "Merge sort" (whnf Merge.sort xs)

mergePar :: Vector Int -> Benchmark
mergePar xs = bench "Parallel merge sort" (whnf MergePar.sort xs)

benchForSize :: GenIO -> Int -> IO Benchmark
benchForSize g n = do
  xs0 <- uniformVector g n
  let !xs = convert xs0 :: Vector Int
  let tests = if n <= 1000 then [binaryInsertion xs, merge xs, mergePar xs, insertion xs]
	else [merge xs, mergePar xs]
  return $ bgroup (show n) tests

main = withSystemRandom $ \ g -> 
  defaultMain =<< mapM (benchForSize g) [10, 100, 1000, 1000000]