module Benchmark where

import Criterion.Main
import System.Random.MWC

import Data.Vector.Primitive

import qualified Data.Vector.Sort.Insertion.Binary as BI

binaryInsertion :: Vector Int -> Benchmark
binaryInsertion xs = bench "Binary insertion sort" (whnf BI.sort xs)

main = withSystemRandom $ \ g -> do
  xs0 <- uniformVector g 100
  let !xs = convert xs0 :: Vector Int
  defaultMain [binaryInsertion xs]