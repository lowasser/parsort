{-# LANGUAGE BangPatterns #-}
{-# OPTIONS -funbox-strict-fields #-}
module Data.Vector.Sort.Tim.Hop (gallopLeft) where

import Data.Bits
import Data.Vector.Generic

import Data.Vector.Sort.Types

import Prelude hiding (length)

data Interval = Interval !Int !Int

index :: Vector v a => v a -> Int -> a
index = (!)

gallopLeft :: Vector v a => LEq a -> a -> v a -> Int -> Int
gallopLeft (<=?) key xs !hint
  | key <=? index xs hint
  	= case hopLeft (<=?) key (unsafeTake hint xs) of
	    Interval l r -> binarySearchL (<=?) key xs (l+1) r
  | otherwise = case hopRight (<=?) key (unsafeDrop hint xs) of
	    Interval l r -> binarySearchL (<=?) key xs (l + hint + 1) (r + hint)
  where	!n = length xs

{-# INLINE binarySearchL #-}
binarySearchL :: Vector v a => LEq a -> a -> v a -> Int -> Int -> Int
binarySearchL (<=?) key xs lo hi = bin lo hi where
  bin !lo !hi
    | lo < hi	= let !m = lo + (hi - lo) `shiftR` 1 in
	if key <=? index xs m then bin lo m else bin (m+1) hi
    | otherwise	= lo

hopLeft :: Vector v a => LEq a -> a -> v a -> Interval
hopLeft (<=?) key xs = hop 0 1 where
  !n = length xs
  hop !lastOff off
    | off <= n, key <=? index xs (n - off)
    	= hop off (off `shiftL` 1 + 1)
    | otherwise
    	= Interval (n - off) (n - lastOff)

hopRight :: Vector v a => LEq a -> a -> v a -> Interval
hopRight (<=?) key xs = hop 0 1 where
  !n = length xs
  a >? b = not (a <=? b)
  hop !lastOff off
    | off < n, key >? index xs off
	= hop off (off `shiftL` 1 + 1)
    | otherwise
	= Interval lastOff (off `min` n)