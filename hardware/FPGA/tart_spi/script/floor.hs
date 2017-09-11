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
import Data.Bool
import Data.Bits
import Data.List
import Data.Maybe
import Data.Tuple
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


-- * Names of design blocks.
------------------------------------------------------------------------------
-- | Local correlator (Xilinx "distributed") SRAM's.
cosRAM     = "COSRAM" :: String
sinRAM     = "SINRAM" :: String

-- | Visibilities read-back SRAM's.
visRAM0    = "SRAM0" :: String
visRAM1    = "SRAM1" :: String

-- | Block SRAM's, and MUX's, for correlator-blocks.
corRAM     = "VISRAM" :: String
corMUX     = "DMUX"   :: String -- TODO: currently ignored

dataMUX    = "DMUX"  :: String

correlator = "CORN"  :: String
blockDSP   = "BDSP"  :: String
blockSDP   = "BSDP"  :: String

dspINST    = "CS0/DSPCS" :: String


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
rangeX p lab l0 l1
  | l0 == l1  = placeX p l0 l'
  | otherwise = "INST " ++ show (p ++ '/':l') ++ loc
  where
    loc = ' ':showLOC l0 ++ ':':drop 4 (showLOC l1)
    l'  = lab ++ "<*>"

-- | Constrain to use two FD's (per LE) from the same SLICE.
dblFD :: Z -> Prefix -> LOC -> [String]
dblFD w p l@(SLICE x y) =
  let (w', u ) = (pred w `shiftR` 2, SLICE x (y+w'))
      (l0, l1) = ("DBL_FD0/FD0", "DBL_FD0/FD1")
  in  [rangeX p l0 l u, rangeX p l1 l u]


-- * Multi-place functions.
------------------------------------------------------------------------------
placeMUX :: Width -> LOC -> Prefix -> String
placeMUX w (SLICE x y) p = toUCF $ zipWith (++) ix ls
  where
    ix = map pf [0..w-1]
    ls = map ((' ':) . showLOC . SLICE x . (y+) . (`shiftR` 1)) [0..w-1]
    pf = \i -> "INST " ++ show (p ++ '/':dataMUX ++ "/MUXF7" ++ showIndex i)

placeRAM32M :: Width -> Label -> LOC -> Z -> Prefix -> String
placeRAM32M w l (SLICE x y) c p = toUCF $ zipWith (++) ix ls
  where
    w' = (w+5) `div` 6
    ix = map pf [0..w'-1]
    ls = map ((' ':) . showLOC . slice x . (y+)) [0..w'-1]
    p' = p ++ '/':correlator ++ show c
    pf = \i -> "INST " ++ show (p' ++ '/':l ++ showIndex i ++ "/RAM32M0")

placeRAMB8 :: Bool -> Width -> LOC -> Prefix -> String
placeRAMB8 dbl w (RAMB8 x y) p = toUCF $ zipWith (++) ix ls
  where
    w' = (w `shiftL` 3 + 35) `div` 36
    ix = map pf [0..w'-1]
    hx = [ bool j (j+j) dbl | j <- [0..w'-1] ]
    ls = map ((' ':) . showLOC . RAMB8 x . (y+)) hx
    pf = \i -> "INST " ++ show (p ++ showIndex i ++ "/SRAM0")

placeDSP48 :: LOC -> Prefix -> String
placeDSP48 (DSP48 x y) p = toUCF $ zipWith (++) ix ls
  where
    ix = map pf [0..3]
    ls = map ((' ':) . showLOC . DSP48 x . (y+)) [0..3]
    cl = '/':dspINST
    pf = \i -> "INST " ++ show (p ++ '/':correlator ++ show i ++ cl)


-- * Some functional-unit placement stuff.
------------------------------------------------------------------------------
-- | Needs to know the number of DSP48's that are being used.
floorDSP :: Prefix -> Z -> String
floorDSP p n =
  let ix = case n of
        4 -> [0,4..12]
        _ -> [1,5..4*n-1]
      bs = [0..n-1]
      ps = [ p ++ show i ++ '/':blockDSP | i <- bs ]
      o  = bool 4 0 (n == 4) :: Z
      dx = zipWith (\i -> placeDSP48 (DSP48 0 i)) ix ps
      rx = zipWith (\i -> let y = i `shiftL` 4 + o
                          in  \q -> floorRAMD q (SLICE 4 y)) bs ps
      ax = zipWith floorAddr ps [SLICE 5 i | i <- [8,16..]]
  in  concat (dx `mix` rx)
--   in  concat (dx `mix` rx) ++ concat ax

------------------------------------------------------------------------------
-- | Floorplans the (standard) correlators.
--   NOTE: Needs to know the number of DSP48's that are being used.
floorSDP :: Prefix -> Z -> String
floorSDP p d =
  let n  = 6 - d
--       ps = [printf "%s%d/CORRELATOR%d/CORR_COS_SIN0" p i j | i <- [d..5], j <- [0..3]]
--       ls = [ SLICE 18 (i*6+2) | i <- [0..4*n-1] ]
--       ax = zipWith floorADD  ps ls
      dbl = False -- heightSDP >= 24
      ax = [""]
      ds = [ SLICE 16 (i*hl+2) | i <- [0..pred n] ]
      qs = [ p ++ show i ++ '/':blockSDP | i <- [d..5] ]
      rs = zipWith floorRAMD qs ds
      (mo, hb, hl) = (hl `shiftR` 2 + 2, hl `shiftR` 1, heightSDP)
      bx = [ floorBLK dbl (p ++ show (d+i) ++ '/':blockSDP) (RAMB8 1 (i*hb+4)) (SLICE 22 (i*hl+mo)) | i <- [0..n-1] ]
  in  concat $ ax ++ rs ++ bx

heightSDP :: Z
heightSDP  = 24

-- | Floorplans the adders (of the correlators) by floorplanning their output
--   registers.
--   FIXME: Currently the labelling is broken.
floorADD :: Prefix -> LOC -> String
floorADD p (SLICE x y) =
  let cx = (SLICE  x    y, SLICE  x    (y+5))
      sx = (SLICE (x+4) y, SLICE (x+4) (y+5))
      fd = zipWith (\l (a, b) -> rangeX p l a b) ["RCOS", "RSIN"] [cx, sx]
--       sd = map (++ " SOFT") fd
--   in  toUCF sd
  in  toUCF fd

------------------------------------------------------------------------------
-- | Needs to know the number of DSP48's that are being used?
--   TODO: Needs to support non-DSP correlators?
floorRAMD :: Prefix -> LOC -> String
floorRAMD p (SLICE x y) =
  let (bs, ys) = ([0..3], [ y + j*4 | j <- bs ])
      (cx, sx) = (map (SLICE x) ys, map (SLICE (x+4)) ys)
      ci = zipWith3 (placeRAM32M 24 cosRAM) cx bs $ repeat p
      si = zipWith3 (placeRAM32M 24 sinRAM) sx bs $ repeat p
  in  concat $ ci `mix` si

-- | Place an address-generation unit at the given (origin) location.
floorAddr :: Prefix -> LOC -> String
floorAddr p l@(SLICE x y) =
  let (y', p') = (y + pred 4 `shiftR` 2, p ++ "/RMW0")
      rs = rangeX p' "FD0" l (SLICE x y')
      ds = dblFD 4 p' (SLICE (x+2) y)
  in  toUCF $ rs:ds

------------------------------------------------------------------------------
-- | Places block RAM's, and their output MUX's, at locations that are
--   determined from the inputs.
floorBLK :: Bool -> Prefix -> LOC -> LOC -> String
floorBLK dbl p ram mux = bx ++ mx
  where
    bx = placeRAMB8 dbl 24 ram $ p ++ "/VISRAM"
    mx = floorMUX p mux

-- | Needs to know the number of DSP48's that are being used.
floorRAMB :: Prefix -> Z -> String
floorRAMB p n = concat $ bi ++ mi
  where
    o  = bool 1 0 (n == 4) :: Z
    bx = map (RAMB8 0) [ shiftL i 3 + o     | i <- bs ]
    mx = map (slice 6) [ shiftL i 4 + o*4+2 | i <- bs ]
    ps = [ p ++ show i ++ '/':blockDSP | i <- bs ]
    pb = [ q ++ '/':corRAM | q <- ps ]
    bi = zipWith (placeRAMB8 False 24) bx pb
    mi = zipWith (placeMUX 24) mx ps
    bs = [0..n-1]

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
  "\n# Instance `AREA_GROUP's:\n" ++
  instsGRP  p d ++
  []

-- | Construct the `AREA_GROUP's.
instsGRP :: Prefix -> Z -> String
instsGRP p d =
  let ix = [printf "INST \"%s%d\" AREA_GROUP=\"cblk%d\";" p i i | i <- [0..5 :: Z]]
      jk = [(2, 15), (16, 31), (32, 47), (48 :: Z, 61 :: Z)]
      dx = take d [(SLICE 0 j, SLICE 11 k) | (j, k) <- jk]
      js = [0,heightSDP..]
      sx = [(SLICE 12 (j+2), SLICE 23 (j+heightSDP+1)) | j <- js]
      lx = [(0 :: Z)..] `zip` take 6 (dx ++ sx)
      l :: (LOC, LOC) -> String
      l (SLICE x y, _) = printf "SLICE_X%dY%d" x y
      r  = l . swap
      px = [printf "AREA_GROUP \"cblk%d\" RANGE=%s:%s;" i (l x) (r x) | (i,x) <- lx]
  in  unlines ix ++ '\n':unlines px

floortest :: String
floortest  =
  "# DSP48 Placement:\n" ++
  placeDSP48  (DSP48 0 1) "" ++
  "# RAMB8 Placement:\n" ++
  placeRAMB8 False 24 (RAMB8 0 2) "/VISRAM" ++
  "# Cosine SRAM Placement:\n" ++
  placeRAM32M 24 cosRAM (SLICE 4 4) 0 "" ++
  "# Sine SRAM Placement:\n" ++
  placeRAM32M 24 sinRAM (SLICE 8 4) 0 "" ++
  "# Placement of the MUX's for the RAMB8 outputs:\n" ++
  placeMUX 24 (SLICE 6 6) "" ++
  []


-- * Helper functions.
------------------------------------------------------------------------------
mix :: [a] -> [a] -> [a]
mix (l:ls) (r:rs) = l:r:mix ls rs
mix    []     rs  = rs
mix    ls     []  = ls

col :: Z -> LOC -> [LOC]
col n (SLICE x y) = SLICE x . (y+) <$> [0..pred n]

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
  let p = maybe "DSP/COR/CXB" toString mp
      d = fromMaybe 4 md
--       u = fromString $ floorplan p d
      u = select $ fromString <$> lines (floorplan p d)
  maybe (stdout u) (`output` u) mo
--   output o pz
--  stdout ""
