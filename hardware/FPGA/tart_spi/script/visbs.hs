#!/usr/bin/env runhaskell
{-# LANGUAGE OverloadedStrings, TypeOperators #-}

------------------------------------------------------------------------------
-- |
-- Module      : Main
-- Copyright   : (C) Tim Molteno     2016
--               (C) Max Scheel      2016
--               (C) Patrick Suggate 2016
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
-- Generates floorplans for TART's hardware correlators, and targeting the
-- Xilinx Spartan 6, LX9.
-- 
-- Changelog:
--  + 27/07/2016  --  initial file;
-- 
-- FIXME:
-- 
-- TODO:
-- 
------------------------------------------------------------------------------

module Main where

import Prelude hiding (FilePath)
import Control.Arrow
import GHC.Word
import Data.Bool
import Data.Bits
import Data.List
import Data.Maybe
import Data.Monoid
-- import Data.IntSet (IntSet)
-- import qualified Data.IntSet as Set
-- import Data.IntMap (IntMap)
-- import qualified Data.IntMap as Map
import Data.Text (pack, unpack)
import Text.Printf
import Turtle hiding (printf)


mfsr8 :: Word8 -> Word8
mfsr8 w = let tap0 = w .&. 0x02
              tap1 = w `shiftR` 4 .&. 0x04
              taps = tap0 .|. tap1
          in  w `rotateL` 1 `xor` taps

mfsr32 :: Word32 -> Word32
mfsr32 w = let tap0 = w .&. 0x02
               tap1 = w `shiftR` 26 .&. 0x04
               taps = tap0 .|. tap1
           in  w `rotateL` 1 `xor` taps


main :: IO ()
main  = do
  let go x = x:go (mfsr8 x)
--   let go x = x:go (mfsr32 x)
--   mapM_ print . take 40 $ go 1
  print . take 256 $ go 1
  print . nub . take 256 $ go 1
