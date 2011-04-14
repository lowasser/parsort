{-# LANGUAGE BangPatterns, CPP #-}
{-# OPTIONS -funbox-strict-fields #-}
module Data.Vector.Sort.Tim.Hop (gallopLeft, gallopRight) where

import Data.Bits

import Data.Vector.Sort.Types

import GHC.Exts

import Prelude hiding (length, take, drop)

offCont :: Int -> (Int -> Int -> a) -> Int -> Int -> a
offCont !off cont a b = cont (a + off) (b + off)

{-# SPECIALIZE gallopLeft :: LEq a -> a -> VVector a -> Int -> Int #-}
{-# SPECIALIZE gallopRight :: LEq a -> a -> VVector a -> Int -> Int #-}
gallopLeft, gallopRight :: Vector v a => LEq a -> a -> v a -> Int -> Int
gallopLeft (<=?) key xs !hint
  | checkIndex hint xs $ key <=? index xs hint
  		= hopLeft (key <=?) (take hint xs) go
  | otherwise	= hopRight (key >?) (drop hint xs) (offCont hint go)
  where	a >? b = not (a <=? b)
	go l r = binarySearchL (key <=?) xs (l+1) r

gallopRight (<=?) key xs !hint = inline gallopLeft (<?) key xs hint
  where a <? b = not (b <=? a)

{-# INLINE binarySearchL #-}
binarySearchL :: Vector v a => (a -> Bool) -> v a -> Int -> Int -> Int
binarySearchL p xs lo hi = bin lo hi where
  bin !lo !hi = checkRange lo hi xs $
    if lo < hi then
	let !m = lo + (hi - lo) `shiftR` 1 in
	  if p (index xs m) then bin lo m else bin (m+1) hi
    else lo

{-# INLINE hopLeft #-}
hopLeft :: Vector v a => (a -> Bool) -> v a -> (Int -> Int -> b) -> b
hopLeft p xs cont = hop 0 1 where
  !n = length xs
  hop !lastOff off
    | off > n	= cont (-1) (n - lastOff)
    | p (index xs (n - off))
    	= hop off (off `shiftL` 1 + 1)
    | otherwise
    	= cont (n - off) (n - lastOff)

{-# INLINE hopRight #-}
hopRight :: Vector v a => (a -> Bool) -> v a -> (Int -> Int -> b) -> b
hopRight p xs cont = hop 0 1 where
  !n = length xs
  hop !lastOff off
    | off < n, p (index xs off)
	= hop off (off `shiftL` 1 + 1)
    | otherwise
	= cont lastOff (off `min` n)