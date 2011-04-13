module Data.Vector.Sort.Tim.Hop.Tests where

import Control.Monad.ST

import Test.QuickCheck

import qualified Data.List as L
import Data.Vector.Primitive

import Data.Vector.Sort.Tim.Hop
import Data.Vector.Algorithms.Search

main = quickCheck (\ x (NonEmpty xs0) -> let xs = (fromList (L.sort xs0)) :: Vector Int in
  gallopLeft (<=) x xs 0 == runST (do
    mv <- thaw xs
    binarySearchL mv x))