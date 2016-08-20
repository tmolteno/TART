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
-- Generates the configuration values for TART's hardware correlators.
-- 
-- Changelog:
--  + 16/06/2016  --  initial file;
-- 
-- FIXME:
-- 
-- TODO:
-- 
------------------------------------------------------------------------------

module Main where

import Prelude hiding (FilePath)
import Data.Bits
import Data.List
import Data.Maybe
import Data.Text (pack, unpack)
import Text.Printf
import Turtle hiding (printf)


type Z = Int

------------------------------------------------------------------------------
-- | Generate all combinations of unique pairs of antennae.
pairs :: Z -> [(Z, Z)]
pairs n = go 0
  where
    go i | i  <  n   = repeat i `zip` [i+1..n-1] ++ go (i+1)
         | otherwise = []

-- | Generate all unique pairwise interactions for the two blocks of antenna
--   indices.
block :: [Z] -> [Z] -> [(Z, Z)]
block (m:ms) ns = repeat m `zip` ns ++ block ms ns
block    []   _ = []

-- | Generate the set of all antenna-index blocks.
blockset :: Z -> Z -> [([Z], [Z])]
blockset a b = go bs
  where
    bs = chunk b [0..a-1]
    go (_:[]) = []
    go (x:xs) = repeat x `zip` xs ++ go xs

------------------------------------------------------------------------------
-- | Generate all unique pairs of antennae indices, for the given number of
--   antennae, and block-size.
blocks :: Z -> Z -> [[(Z, Z)]]
blocks a b = go bs
  where
    bs = chunk b [0..a-1]
    go (_:[]) = []
    go (x:xs) = map (block x) xs ++ go xs

shareds :: Z -> Z -> [(Z, Z)]
shareds a b =
  let bs = concat $ blocks a b
      ps = pairs a
  in  ps \\ bs

-- | Choose blocks to put the shared pair-wise interactions.
choices :: [([Z], [Z])] -> [(Z, Z)] -> [[(Z, Z)]]
choices bz sz =
  let go (a, b) = let bs = a ++ b
                  in  filter (\(i, j) -> elem i bs || elem j bs) sz
  in  map go bz

common :: [Z] -> [(Z, Z)] -> [(Z, Z)]
common xs = filter (\(i, j) -> elem i xs || elem j xs)

-- TODO: Determine the quantity to take; i.e., don't hard-code to `5`!
allocate :: Z -> ([Z], [Z]) -> [(Z, Z)] -> ([(Z, Z)], [(Z, Z)])
allocate n (a, b) sz =
  let xs = take n (common a sz) ++ take n (common b sz)
  in  (xs, sz \\ xs)


-- * Correlator-block index-set functions.
------------------------------------------------------------------------------
-- | Build the blocked set of antenna indices, for TART's correlators.
buildset :: Z -> Z -> [[(Z, Z)]]
buildset a b =
  let (n,bz) = (length bz, blockset a b)
      (m,sz) = (length sz, shareds  a b)
      (c,wz) = (m `div` (n*2), blocks a b)
      (xz,s) = foldl (\(xs, s) b ->
                       let (x, s') = allocate c b s
                       in  (x:xs, s')) ([], sz) bz
--   in  map sort $ zipWith (++) (reverse xz) wz
  in  zipWith (++) wz (reverse xz)

------------------------------------------------------------------------------
-- | Generate the Verilog parameters, for the correlator-blocks.
params :: Z -> Z -> Z -> Shell Text
params a b m =
  let bs = buildset a b
      bi = blockset a b
--       bz = map (chunk m) bs
      bz = map (fmap (adjust m) . chunk m) bs
      ps = pairs a
      -- ^ number of index-bits:
--       ib = ceiling $ log (fromIntegral (b+b)) / log 2
      ib = ceiling $ log (fromIntegral a) / log 2
      -- ^ number of parameter-bits/correlator:
      pb = ib*2*m
      cs = concat (concat bz)
--       cs = concat bs
      ix = catMaybes $ map (flip elemIndex cs) ps
      c  = 2^ib
      l  = length (head bz)     -- #correlators/block
      -- ^ generate antenna-index parameter labels:
      lx = [ printf "PAIRS%02x_%02x = " i j | i <- [0..length bs-1], j <- [0..l-1] ]
      -- ^ generate antenna-index parameters:
      hx x = printf "{%d'h%x};" pb x :: String
      px = concatMap (map (hx . toBitfield ib)) bz
      pp = zipWith (++) (repeat "   parameter ") (zipWith (++) lx px)
      hd = printf "   //\n   // Generated using:\n   //   ./pairs.hs -a %d -b %d -m %d -o <file>\n   //" a b m
  in  select $ pack <$> (hd:pp)

toBitfield :: Z -> [(Z, Z)] -> Integer
toBitfield b = go 0
  where
    go _        []  = 0
    go i ((p,q):qs) = x .|. go (i+a) qs
      where
        x = fromIntegral (q `shiftL` b .|. p) `shiftL` i
    a  = b+b

-- | Generate the ROM contexts for the `index -> correlator` redirection unit.
--   TODO: Use "reverse-indices" so that burst-reads can be supported? I.e.,
--     read the entire SRAM contents of each correlator, and remap to the
--     appropriate address?
indices :: Z -> [[[(Z, Z)]]] -> [(Z, Z)] -> String
indices b bz ps = show ix
  where
    c  = 2^b
--     ix = map (flip shiftR b) $ catMaybes $ map (flip elemIndex cs) ps
    ix = catMaybes $ map (flip elemIndex cs) ps
    cs = concat $ concatMap (map (adjust c)) bz


-- * Helper functions.
------------------------------------------------------------------------------
-- | Break a list into chunks of the given size.
chunk :: Z -> [a] -> [[a]]
chunk s xs
  | null zs   = [ys]
  | otherwise = ys:chunk s zs
  where
    (ys, zs) = (take s xs, drop s xs)

-- | Repeat elements until the list is the given length, if possible.
adjust :: Z -> [a] -> [a]
adjust s xs | null xs   = []
            | n  <  s   = adjust s $ xs ++ take (s-n) xs
            | otherwise = xs
  where
    n = length xs


-- * Program main.
------------------------------------------------------------------------------
-- | Parse command-line options.
parser :: Parser (Z, Z, Z, FilePath)
parser  = (,,,) <$> optInt  "antennas"  'a' "The number of antennae"
                <*> optInt  "blocksize" 'b' "The size of the antenna blocks"
                <*> optInt  "multiplex" 'm' "The time-multiplexing ratio"
                <*> optPath "outfile"   'o' "Output filename"

------------------------------------------------------------------------------
-- | Generate antenna-pair indices, for the correlators.
--   Default (Makefile) options:
--     runhaskell script/pairs.hs --antennas=24 --blocksize=6 --multiplex=12 \
--       --outfile include/tart_pairs.v
--   
main :: IO ()
main  = do
  stdout " == Antenna pair generation =="
  (a, b, m, o) <- options "" parser
  let bz = blockset a b
      sz = shareds  a b
      pz = params a b m
      sho ((s, t), ps) = printf "{%s, \t%s}:\t(%d)%s\n\n" (show s) (show t) (length ps) (show ps)

--   print $ pairs a
--   print $ length $ pairs a
--   stdout "\nBlocks:"
--   mapM_ print $ blocks a b
--   stdout "\nBlocksets:"
--   mapM_ print bz

--   stdout "\nShared index pairs:"
--   mapM_ sho $ bz `zip` chunk 10 sz

--   stdout "\nCorrelator block indices:"
--   mapM_ (print . length) $ choices bz sz
--   mapM_ print $ choices bz sz
--   mapM_ sho $ bz `zip` buildset a b
--   mapM_ print $ concat $ fmap (adjust 12) . chunk 12 <$> buildset a b

  stdout "\nCorrelator block parameters:"
  stdout pz
  output o pz
  stdout ""
