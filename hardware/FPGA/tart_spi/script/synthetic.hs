#!/usr/bin/env runhaskell
{-# LANGUAGE OverloadedStrings, TypeOperators,
             DeriveFunctor, DeriveFoldable, DeriveTraversable
  #-}

------------------------------------------------------------------------------
-- |
-- Module      : Main
-- Copyright   : (C) Tim Molteno     2017
--               (C) Max Scheel      2017
--               (C) Patrick Suggate 2017
-- License     : GPL3
-- 
-- Maintainer  : Patrick Suggate <patrick.suggate@gmail.com>
-- Stability   : Experimental
-- Portability : non-portable
-- 
-- 
-- This file is part of TART.
-- 
-- TART is free software: you can redistribute it and/or modify it under the
-- terms of the GNU Lesser Public License as published by the Free Software
-- Foundation, either version 3 of the License, or (at your option) any later
-- version.
-- 
-- TART is distributed in the hope that it will be useful, but WITHOUT ANY
-- WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE.  See the GNU Lesser Public License for more
-- details.
-- 
-- You should have received a copy of the GNU Lesser Public License along with
-- TART.  If not, see <http://www.gnu.org/licenses/>.
-- 
-- 
-- Description:
-- Generates synthetic visibilities data, to be compared with the outputs of
-- TART's DSP functional-unit.
-- 
-- NOTE:
--  + requires `text` and `turtle` to be installed:
--      > cabal install text turtle
--      > make pairs
--      > make permute
-- 
-- Changelog:
--  + 14/06/2017  --  initial file;
-- 
-- FIXME:
-- 
-- TODO:
-- 
------------------------------------------------------------------------------

module Main where

import qualified System.Environment as Sys
import Data.Word
import Data.Bool
import Data.List (tails, intercalate)
import Data.Bits
import Text.Printf


-- * Functions to compute the visibilities.
------------------------------------------------------------------------------
visb :: [Bool] -> [Bool] -> [([Int],[Int])] -> [([Int],[Int])]
visb (b:bs) (_:js) ((r,i):vs) = (corr b bs r, corr b js i):visb bs js vs
visb     _      _          _  = []

corr :: Bool -> [Bool] -> [Int] -> [Int]
corr b bs cs = zipWith (bool id succ) (map (b ==) bs) cs

incr :: [Bool] -> [Bool]
incr        []  = [True]
incr (False:bs) = True :bs
incr (True :bs) = False:incr bs

------------------------------------------------------------------------------
-- | Calculate the visibilies by correlating for `n` samples.
calc :: Int -> [[Bool]] -> ([[Bool]], [Int])
calc n bz =
  let go (a:b:bs) vs = let vs' = visb b a vs in vs':go (b:bs) vs'
      l  = length $ head bz
      vs = map (\x -> (x, x)) $ init $ tails $ replicate l 0
      xs = go bz vs!!pred n
      ms = mean $ take n $ tail bz
      ws = concat $ uncurry zip <$> xs
      ys = concat $ foldr (\(x,y) zs -> [x,y]:zs) [] ws :: [Int]
  in  (drop n bz, ys ++ ms)
  

-- * Helper functions.
------------------------------------------------------------------------------
mask :: Int -> Int -> [Bool]
mask l m = replicate m True ++ replicate (l-m) False

mean :: [[Bool]] -> [Int]
mean bz = go bz $ replicate n 0
  where
    n   = length bz
    go (b:bs) cs = bs `go` zipWith (bool id succ) b cs
    go    []  cs = cs

dump :: [Int] -> IO ()
dump vs = let ts  = printf "%06x " <$> vs :: [String]
              go _ [] = []
              go n xs = take n xs:go n (drop n xs)
              ts' = map unlines $ go 8 $ ('\t':) . concat <$> go 12 ts :: [String]
          in  putStrLn $ intercalate "\n" ts'

------------------------------------------------------------------------------
-- | Shift-register to use as a pseudorandom signal-source.
mfsr32 :: Word32 -> Word32
mfsr32 w = let tap0 = w .&. 0x02
               tap1 = w `shiftR` 26 .&. 0x04
               taps = tap0 .|. tap1
           in  w `rotateL` 1 `xor` taps

tobits :: Word32 -> [Bool]
tobits w = let go 32 _ = []
               go  n x = (x .&. 1 == 1):go (n+1) (x `shiftR` 1)
           in  go 0 w


-- * Generate visibilities from synthetic inputs.
------------------------------------------------------------------------------
-- | Compute synthetic visibilities, and with input arguments:
--    `l`  --  number of antennae (or, length of bit-strings);
--    `m`  --  number of mask-bits, for testing subranges of the input;
--    `e`  --  exponent of the number of iterations; and
--    `d`  --  number of initial samples to drop.
--   For example, to duplicate the behaviour of `tart_dsp_tb`, `d = 2`, and
--   `l = 24`.
main :: IO ()
main  = do
  args <- Sys.getArgs
  let (l:m:e:d:_) = read <$> args
      cz = drop d $ map (zipWith (&&) ms) $ iterate incr $ replicate l False
      rz = drop d $ map (take l . tobits) $ iterate mfsr32 1
      bz = rz
      ms = mask l m
      n  = 2^e
      (bz' , xs) = calc n bz
      (bz'', ys) = calc n bz'
  printf "Synthetic visibilities data-generator (%d, %d, %d, %d):\n" l m n d
--   print xs
--   print ys
  printf "\n\nBank 0:\n"
  dump xs
  printf "\n\nBank 1:\n"
  dump ys
