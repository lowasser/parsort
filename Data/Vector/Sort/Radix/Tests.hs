{-# LANGUAGE CPP #-}
module Data.Vector.Sort.Radix.Tests (main) where

import Test.QuickCheck

import Data.List (reverse)
import Data.Vector.Sort.Radix.Class

import Data.Int
import Data.Word

testRadix :: (Ord a, Radix a) => a -> a -> Bool
testRadix a b = compare a b == compare aRep bRep
  where aRep = reverse [radix p a | p <- [0..passes a - 1]]
	bRep = reverse [radix p b | p <- [0..passes b - 1]]

#define TEST(ty) printTestCase "ty" (testRadix :: ty -> ty -> Bool)

main = quickCheck $ conjoin
  [TEST(Int32), TEST(Word32), TEST(Word64), TEST(Int64), TEST(Int), TEST(Word)]