{-# LANGUAGE ImplicitParams #-}
module Data.Vector.Sort.Tim.Hop.Tests where

import Control.Monad.ST

import Test.QuickCheck

import qualified Data.List as L
import Data.Maybe
import Data.Vector.Primitive

import Data.Vector.Sort.Tim.Hop
import Data.Vector.Sort.Comparator
import Data.Vector.Algorithms.Search

import Prelude hiding (length, reverse)

tests = [
  \ x (NonEmpty xs0) (NonNegative i) -> let xs = (fromList (L.sort xs0)) :: Vector Int in
  printTestCase "gallopLeft" $ gallopLeft x xs (i `rem` length xs) == fromMaybe (length xs) (findIndex (>= x) xs),
  \ x (NonEmpty xs0) (NonNegative i) -> let xs = (fromList (L.sort xs0)) :: Vector Int in
  printTestCase "gallopRight" $ gallopRight x xs (i `rem` length xs) == fromMaybe (length xs) (findIndex (> x) xs)]
  where ?cmp = mkComparator (<=)

main = quickCheck (conjoin tests)