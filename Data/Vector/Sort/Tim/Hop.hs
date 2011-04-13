{-# LANGUAGE BangPatterns #-}
{-# OPTIONS -funbox-strict-fields #-}
module Data.Vector.Sort.Tim.Hop (gallopLeft, gallopRight) where

import Control.Exception.Base

import Data.Bits
import Data.Vector.Generic

import Data.Vector.Sort.Types

import Prelude hiding (length)

data Interval = Interval !Int !Int

index :: Vector v a => v a -> Int -> a
index = (!)

asserter :: Show a => Bool -> a -> b -> b
asserter True _ b = b
asserter False x _ = error (show x)

{-# SPECIALIZE gallopLeft :: LEq a -> a -> VVector a -> Int -> Int #-}
{-# SPECIALIZE gallopRight :: LEq a -> a -> VVector a -> Int -> Int #-}
gallopLeft, gallopRight :: Vector v a => LEq a -> a -> v a -> Int -> Int
gallopLeft (<=?) key xs !hint
  | key <=? index xs hint
  	= case hopLeft (key <=?) (unsafeTake hint xs) of
	    Interval l r -> go l r
  | otherwise = case hopRight (key >?) (unsafeDrop hint xs) of
	    Interval l r -> go (l + hint) (r + hint)
  where	a >? b = not (a <=? b)
	go l r = asserter (l >= -1 && l < r && r <= length xs) (l, r, length xs)
		    $ binarySearchL (key <=?) xs (l+1) r

gallopRight (<=?) key xs !hint
  | key <? index xs hint
  	= case hopLeft (key <?) (unsafeTake hint xs) of
	    Interval l r -> go l r
  | otherwise = case hopRight (<=? key) (unsafeDrop hint xs) of
	    Interval l r -> go (l+hint) (r+hint)
  where	a <? b = not (b <=? a)
	go l r = assert (l >= -1 && l < r && r <= length xs) $ binarySearchL (key <?) xs (l+1) r

{-# INLINE binarySearchL #-}
binarySearchL :: Vector v a => (a -> Bool) -> v a -> Int -> Int -> Int
binarySearchL p xs lo hi = bin lo hi where
  bin !lo !hi
    | if not (0 <= lo &&  lo <= hi && hi <= length xs) then error $ show (lo, hi, length xs) else
	lo < hi	= let !m = lo + (hi - lo) `shiftR` 1 in
	  if p (index xs m) then bin lo m else bin (m+1) hi
    | otherwise	= lo

{-# INLINE hopLeft #-}
hopLeft :: Vector v a => (a -> Bool) -> v a -> Interval
hopLeft p xs = hop 0 1 where
  !n = length xs
  hop !lastOff off
    | off <= n, p (index xs (n - off))
    	= hop off (off `shiftL` 1 + 1)
    | otherwise
    	= Interval (max (-1) (n-off)) (n - lastOff)

{-# INLINE hopRight #-}
hopRight :: Vector v a => (a -> Bool) -> v a -> Interval
hopRight p xs = hop 0 1 where
  !n = length xs
  hop !lastOff off
    | off < n, p (index xs off)
	= hop off (off `shiftL` 1 + 1)
    | otherwise
	= Interval lastOff (off `min` n)