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


-- * Floor-planning data-types.
------------------------------------------------------------------------------
data LOC = SLICE Z Z
         | RAMB8 Z Z
         | DSP48 Z Z
         deriving (Eq, Read, Show)

type Z = Int
type Width = Z
type Prefix = String
type Label  = String


-- * Functions to emit UCF text.
------------------------------------------------------------------------------
showIndex :: Z -> String
showIndex n = '<':show n ++ ">"

showLOC :: (Monoid s, IsString s) => LOC -> s
{-# SPECIALISE showLOC :: LOC -> String #-}
showLOC (SLICE x y) = "LOC=SLICE_X" <> fromString (show x ++ 'Y':show y)
showLOC (RAMB8 x y) = "LOC=RAMB8_X" <> fromString (show x ++ 'Y':show y)
showLOC (DSP48 x y) = "LOC=DSP48_X" <> fromString (show x ++ 'Y':show y)

toUCF :: [String] -> String
toUCF  = (++ ";\n\n") . intercalate ";\n"


-- * Some primitive-component placement stuff.
------------------------------------------------------------------------------
placeMUX :: Width -> LOC -> Prefix -> String
placeMUX w (SLICE x y) p = toUCF $ zipWith (++) ix ls
  where
    ix = map pf [0..w-1]
    ls = map ((' ':) . showLOC . SLICE x . (y+) . (`shiftR` 1)) [0..w-1]
    pf = \i -> "INST " ++ show (p ++ "/MUXDAT0/MUXF7" ++ showIndex i)

placeRAM32M :: Width -> Label -> LOC -> Z -> Prefix -> String
placeRAM32M w l (SLICE x y) c p = toUCF $ zipWith (++) ix ls
  where
    w' = (w+5) `div` 6
    ix = map pf [0..w'-1]
    ls = map ((' ':) . showLOC . SLICE x . (y+)) [0..w'-1]
    pf = \i -> "INST " ++ show (p ++ "/CORRELATOR" ++ show c ++ '/':l ++ showIndex i)

placeRAMB8 :: Width -> LOC -> Prefix -> String
placeRAMB8 w (RAMB8 x y) p = toUCF $ zipWith (++) ix ls
  where
    w' = (w `shiftL` 3 + 35) `div` 36
    ix = map pf [0..w'-1]
    ls = map ((' ':) . showLOC . RAMB8 x . (y+)) [0..w'-1]
    pf = \i -> "INST " ++ show (p ++ showIndex i ++ "/SRAM0")

placeDSP48 :: LOC -> Prefix -> String
placeDSP48 (DSP48 x y) p = toUCF $ zipWith (++) ix ls
  where
    ix = map pf [0..3]
    ls = map ((' ':) . showLOC . DSP48 x . (y+)) [0..3]
    cl = "/CORR_COS_SIN0/DSP_COS_SIN0"
    pf = \i -> "INST " ++ show (p ++ "/CORRELATOR" ++ show i ++ cl)


-- * Some functional-unit placement stuff.
------------------------------------------------------------------------------
-- | Needs to know the number of DSP48's that are being used.
floorDSP :: Prefix -> Z -> String
floorDSP p n =
  let ix = case n of
        4 -> [0,4..12]
        _ -> [1,5..4*n-1]
      bs = [0..n-1]
  in  concatMap (\i -> placeDSP48 (DSP48 0 i) p) ix

-- | Needs to know the number of DSP48's that are being used.
floorRAM :: Prefix -> Z -> String
floorRAM p n = concat $ bi ++ ci ++ si ++ mi
  where
    o  = bool 1 0 (n == 4) :: Z
    bx = map (RAMB8 0) [ shiftL i 3 + o     | i <- bs ]
    cx = map (slice 4) [ shiftL i 4 + o*4   | i <- bs ]
    sx = map (slice 8) [ shiftL i 4 + o*4   | i <- bs ]
    mx = map (slice 6) [ shiftL i 4 + o*4+2 | i <- bs ]
    ps = [ p ++ show i | i <- bs ]
    pb = [ q ++ "/VISRAM" | q <- ps ]
    bi = zipWith  (placeRAMB8 24) bx pb
    ci = zipWith (placeRAM32M 24 "RAM32X6_SDP_COS0") cx [0..3] <*> ps -- TODO
    si = zipWith3 (placeRAM32M 24 "RAM32X6_SDP_SIN0") sx [0..3] ps
    mi = zipWith  (placeMUX 24) mx ps
    bs = [0..n-1]

slice :: Z -> Z -> LOC
slice x = SLICE x
-- slice x = fixLOC . SLICE x

fixLOC :: LOC -> LOC
fixLOC (SLICE x y)
  | y <  0 || y > 63               = error $ "Invalid row:\t" ++ show y
  | x <  0 || x > 23               = error $ "Invalid col:\t" ++ show x
  | y <  2 && (x /= 14 || x /= 15) = SLICE (x+o) (y+2)
  | y > 61 && (x /= 14 || x /= 15) = SLICE (x+o) (y-2)
  | otherwise                      = SLICE x y
  where
    o = bool 4 (-4) (x < 8)

-- | Needs to know the number of DSP48's that are being used.
floorMUX :: Prefix -> Z -> String
floorMUX p n = concat $ zipWith (placeMUX 24) lx px
  where
    o  = bool 6 2 (n == 4) :: Z
    lx = map (SLICE 6) [ shiftL i 4 + o | i <- bs ]
    px = [ p ++ show i | i <- bs ]
    bs = [0..n-1]

floorplan :: Z -> String
floorplan d =
  "# DSP48 Placement:\n" ++
  floorDSP p d ++
  "# RAM Placement:\n" ++
  floorRAM p d
  where
    p = "CORRELATOR_BLOCK"

floortest :: String
floortest  =
  "# DSP48 Placement:\n" ++
  placeDSP48  (DSP48 0 1) "" ++
  "# RAMB8 Placement:\n" ++
  placeRAMB8 24 (RAMB8 0 2) "/VISRAM" ++
  "# Cosine SRAM Placement:\n" ++
  placeRAM32M 24 "RAM32X6_SDP_COS0" (SLICE 4 4) 0 "" ++
  "# Sine SRAM Placement:\n" ++
  placeRAM32M 24 "RAM32X6_SDP_SIN0" (SLICE 8 4) 0 "" ++
  "# Placement of the MUX's for the RAMB8 outputs:\n" ++
  placeMUX 24 (SLICE 6 6) ""


-- * Floor-planning program.
------------------------------------------------------------------------------
-- | Parse command-line options.
parser :: Parser (Z, FilePath)
parser  = (,) <$> optInt  "num DSP's" 'd' "The number of DSP48's to use"
              <*> optPath "outfile  " 'o' "Output filename"

main :: IO ()
main  = do
  stdout " == TART floorplanner for the hardware correlators =="
  (d, o) <- options "" parser
  stdout "\nCorrelator block parameters:"
  stdout $ fromString (floorplan d)
--   output o pz
--  stdout ""
