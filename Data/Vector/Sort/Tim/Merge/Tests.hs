{-# LANGUAGE ImplicitParams, TypeSynonymInstances, FlexibleInstances #-}
module Data.Vector.Sort.Tim.Merge.Tests where

import Control.Monad.ST
import Debug.Trace
import Test.QuickCheck

import Data.Vector.Primitive hiding ((++), length)
import qualified Data.Vector.Algorithms.Merge as Sort
import Data.Vector.Sort.Comparator
import qualified Data.Vector.Sort.Tim.Merge as TimMerge
import Data.Vector.Sort.Types

import Prelude hiding  (length)

testMerge :: PVector Int -> Int -> Bool
testMerge xs k = runST $ do
  mv1 <- thaw xs
  mv2 <- thaw xs
  Sort.sort (takeM k mv1)
  Sort.sort (dropM k mv1)
  TimMerge.merge mv1 0 k (lengthM mv1 - k) 7
  Sort.sort mv2
  v1 <- freeze mv1
  v2 <- freeze mv2
  return $ if v1 /= v2 then
      traceShow ("Expected: " ++ show v2 ++ "\nActual: " ++ show v1) False
    else True
  where ?cmp = mkComparator (<=)

instance Arbitrary (PVector Int) where
  arbitrary = fmap fromList arbitrary
  shrink xs = do
    xs' <- shrink (toList xs)
    return (fromList xs')

main = quickCheck $ \ xs (NonNegative k0) -> let
    k = k0 `mod` length xs
    in (length xs > 0 && k > 0 && length xs - k > 0) ==> testMerge xs k