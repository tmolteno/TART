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


-- * Smart-constructors for locations.
------------------------------------------------------------------------------
-- | The internal SLICE layout is not quite a regular grid. There are some
--   missing bits.
slice :: Z -> Z -> LOC
-- slice x = SLICE x
slice x = fixLOC . SLICE x

fixLOC :: LOC -> LOC
fixLOC (SLICE x y)
  | y <  0 || y > 63               = error $ "Invalid row:\t" ++ show y
  | x <  0 || x > 23               = error $ "Invalid col:\t" ++ show x
  | y <  2 && (x /= 14 || x /= 15) = SLICE (x+o) (y+2)
  | y > 61 && (x /= 14 || x /= 15) = SLICE (x+o) (y-2)
  | otherwise                      = SLICE x y
  where
    o = bool 4 (-4) (x < 8)


-- * Some primitive-component placement stuff.
------------------------------------------------------------------------------
-- | Place a generic Xilinx primitive.
placeX :: Prefix -> LOC -> Label -> String
placeX p loc lab = "INST " ++ show (p ++ '/':lab) ++ showLOC loc

rangeX :: Prefix -> Label -> LOC -> LOC -> String
rangeX p lab l0 l1 =
  let loc = ' ':showLOC l0 ++ ':':drop 4 (showLOC l1)
--   in  "INST " ++ show (p ++ '/':lab ++ "[*]") ++ loc
  in  "INST " ++ show (p ++ '/':lab ++ "<*>") ++ loc
--   in  "INST " ++ show (p ++ '/':lab ++ "<0>") ++ loc

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
    ls = map ((' ':) . showLOC . slice x . (y+)) [0..w'-1]
    pf = \i -> "INST " ++ show (p ++ "/CORRELATOR" ++ show c ++ '/':l ++ showIndex i ++ "/RAM32M0")

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
      ps = [ p ++ show i | i <- bs ]
      o  = bool 4 0 (n == 4) :: Z
      dx = zipWith (\i -> placeDSP48 (DSP48 0 i)) ix ps
      rx = zipWith (\i -> let y = i `shiftL` 4 + o
                          in  \q -> floorRAMD q (SLICE 4 y)) bs ps
  in  concat $ dx `mix` rx

-- | Floorplans the (standard) correlators.
--   NOTE: Needs to know the number of DSP48's that are being used.
floorSDP :: Prefix -> Z -> String
floorSDP p d =
  let n  = 6 - d
--       ps = [ p ++ show i ++ "/CORRELATOR" ++ show j | i <- [d..5], j <- [0..3] ]
      ps = [ p ++ show i ++ "/CORRELATOR" ++ show j ++ "/CORR_COS_SIN0" | i <- [d..5], j <- [0..3] ]
      ls = [ SLICE 18 (i*6+2) | i <- [0..4*n-1] ]
      ax = zipWith floorADD  ps ls
      ds = [ SLICE 16 (i*4+2) | i <- [0..3] ]
      qs = [ p ++ show i | i <- [d..5] ]
      rs = zipWith floorRAMD qs ds
      bx = [ floorBLK (p ++ show (d+i)) (RAMB8 1 (i*8+2)) (SLICE 22 (i*16+2)) | i <- [0..n-1] ]
  in  concat $ ax ++ rs ++ bx

-- | Floorplans the adders (of the correlators) by floorplanning their output
--   registers.
floorADD :: Prefix -> LOC -> String
floorADD p (SLICE x y) =
  let cx = (SLICE  x    y, SLICE  x    (y+5))
      sx = (SLICE (x+4) y, SLICE (x+4) (y+5))
--       fd = zipWith (\l (a, b) -> rangeX p l a b) ["r_cos", "r_sin"] [cx, sx]
      fd = zipWith (\l (a, b) -> rangeX p l a b) ["RCOS", "RSIN"] [cx, sx]
  in  toUCF fd

col :: Z -> LOC -> [LOC]
col n (SLICE x y) = SLICE x . (y+) <$> [0..pred n]

-- | Needs to know the number of DSP48's that are being used?
--   TODO: Needs to support non-DSP correlators?
floorRAMD :: Prefix -> LOC -> String
floorRAMD p (SLICE x y) =
  let (bs, ys) = ([0..3], [ y + j*4 | j <- bs ])
      (cx, sx) = (map (SLICE x) ys, map (SLICE (x+4)) ys)
      ci = zipWith3 (placeRAM32M 24 "RAM32X6_SDP_COS") cx bs $ repeat p
      si = zipWith3 (placeRAM32M 24 "RAM32X6_SDP_SIN") sx bs $ repeat p
  in  concat $ ci `mix` si

-- | Needs to know the number of DSP48's that are being used.
floorRAMB :: Prefix -> Z -> String
floorRAMB p n = concat $ bi ++ mi
  where
    o  = bool 1 0 (n == 4) :: Z
    bx = map (RAMB8 0) [ shiftL i 3 + o     | i <- bs ]
    mx = map (slice 6) [ shiftL i 4 + o*4+2 | i <- bs ]
    ps = [ p ++ show i | i <- bs ]
    pb = [ q ++ "/VISRAM" | q <- ps ]
    bi = zipWith (placeRAMB8 24) bx pb
    mi = zipWith (placeMUX 24) mx ps
    bs = [0..n-1]

-- | Places block RAM's, and their output MUX's, at locations that are
--   determined from the inputs.
floorBLK :: Prefix -> LOC -> LOC -> String
floorBLK p ram mux = bx ++ mx
  where
    bx = placeRAMB8 24 ram $ p ++ "/VISRAM"
    mx = floorMUX p mux

-- | Needs to know the number of DSP48's that are being used.
floorMUX :: Prefix -> LOC -> String
floorMUX p loc = placeMUX 24 loc p


-- * Generate floorplanned correlators.
------------------------------------------------------------------------------
floorplan :: Prefix -> Z -> String
floorplan p d =
  "# DSP48-based correlator layout:\n" ++
  floorDSP  p d ++
  "# RAM Placement:\n" ++
  floorRAMB p d ++
  "# Standard adder-based correlator layout:\n" ++
  floorSDP  p d ++
  []

floortest :: String
floortest  =
  "# DSP48 Placement:\n" ++
  placeDSP48  (DSP48 0 1) "" ++
  "# RAMB8 Placement:\n" ++
  placeRAMB8 24 (RAMB8 0 2) "/VISRAM" ++
  "# Cosine SRAM Placement:\n" ++
  placeRAM32M 24 "RAM32X6_SDP_COS" (SLICE 4 4) 0 "" ++
  "# Sine SRAM Placement:\n" ++
  placeRAM32M 24 "RAM32X6_SDP_SIN" (SLICE 8 4) 0 "" ++
  "# Placement of the MUX's for the RAMB8 outputs:\n" ++
  placeMUX 24 (SLICE 6 6) "" ++
  []


-- * Helper functions.
------------------------------------------------------------------------------
mix :: [a] -> [a] -> [a]
mix (l:ls) (r:rs) = l:r:mix ls rs
mix    []     rs  = rs
mix    ls     []  = ls

-- | Convert a string to a `String`.
toString :: Show a => a -> String
toString  = init . tail . show


-- * Floor-planning program.
------------------------------------------------------------------------------
data Settings = Settings { _numDSP  :: Maybe Z
                         , _prefix  :: Maybe Text
                         , _outfile :: Maybe FilePath
                         }

-- | Parse command-line options.
parser :: Parser Settings
parser  =
  Settings
  <$> optional (optInt  "numDSP"  'd' "The number of DSP48's to use")
  <*> optional (optText "prefix"  'p' "Prefix of the correlator unit")
  <*> optional (optPath "outfile" 'o' "Output filename")

main :: IO ()
main  = do
--   stdout " == TART floorplanner for the hardware correlators =="
  Settings md mp mo <- options "" parser
--   stdout "\nCorrelator block parameters:"
  let p = maybe "TART_CORRELATOR0/CORRELATOR_BLOCK" toString mp
      d = fromMaybe 4 md
      u = fromString $ floorplan p d
  maybe (stdout u) (`output` u) mo
--   output o pz
--  stdout ""
