#!/usr/bin/env runhaskell
{-# LANGUAGE OverloadedStrings, TypeOperators,
             DeriveFunctor, DeriveFoldable, DeriveTraversable
  #-}

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
-- Generates the configuration values for TART's hardware correlators.
-- 
-- NOTE:
--  + requires `text` and `turtle` to be installed:
--      > cabal install text turtle
--      > make pairs
--      > make permute
-- 
-- Changelog:
--  + 16/06/2016  --  initial file;
--  + 15/10/2016  --  permutation vector support added;
-- 
-- FIXME:
-- 
-- TODO:
--  + only inputs `a == 24, b == 6, m == 12` are correctly supported;
--  + finish the refactoring (to use `Pair`, etc.);
--  + rewrite in `Python`?
-- 
------------------------------------------------------------------------------

module Main where

import Prelude hiding (FilePath)
import Control.Arrow
import Data.Foldable
import Data.Bool
import Data.List
import Data.Maybe
import Data.Tuple
import Data.Bits
import Data.Text (pack, unpack)
import Text.Printf
import Turtle hiding (printf)

import qualified Data.Set as Set
import qualified Data.Map as Map


-- * Correlator-pairs data-types.
------------------------------------------------------------------------------
-- | A `Pair` can either be a pair of (Re/Im) antenna values to correlate, or
--   a pair of inputs to sum, as part of computing their means.
data Pair  a = Pair a a
             | Mean a a
             deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable)

-- ^ convenience aliases:
type Z = Int

type Pairs  a = [Pair  a]
type Means  a = [Pair  a]
type Block  a = [Pairs a]
type Blocks a = [Block a]

type BlockZ   =  Pair [Z]
type BlockSet = [Pair [Z]]


-- ** Permutation vector functionality.
------------------------------------------------------------------------------
-- | Generate the permutation vector, to reorder the visibilities into the
--   standard "triangular" ordering.
permute :: Z -> Z -> Z -> [Z]
permute a b m =
  let pz = params a b m
      ps = concat $ concat <$> pz
      pm = pairIndex a ps
      go (Pair p q:ps) = p:q:go ps
--       go (Mean p q:ps) = p:q:go ps -- unflipped means
      go (Mean p q:ps) = q:p:go ps -- flip the means
      go           []  = []
  in  revperm $ go pm

revperm :: [Z] -> [Z]
revperm ps =
  let go i =
        let Just j = findIndex (== i) ps
        in  bool (j:go (i+1)) [] (i == length ps)
  in  go 0

-- | The visibilities are stored first, and then followed by the one-counts
--   for each antenna (which is used to compute the means).
--   NOTE: Currently O(n^2).
pairIndex :: Z -> Pairs Z -> Pairs Z
pairIndex stride =
  let f (Pair i j) = let k = ((2*stride - i - 3) * i) + 2*j - 2
                     in  k `Pair` succ k
      f       mij  = (p+) <$> mij
      p = stride * pred stride
  in  map f


-- * Antenna-pair generators.
------------------------------------------------------------------------------
-- | Generate all combinations of unique pairs of antennae.
pairs :: Z -> Pairs Z
pairs n = go 0
  where
    go i | i  <  n   = zipWith Pair (repeat i) [i+1..n-1] ++ go (i+1)
         | otherwise = []

-- | Generate all unique pairwise interactions for the two blocks of antenna
--   indices; i.e., computes the (complete) bipartite graph for the two sets
--   of nodes.
block :: [Z] -> [Z] -> Pairs Z
block (m:ms) ns = zipWith Pair (repeat m) ns ++ block ms ns
block    []   _ = []

------------------------------------------------------------------------------
-- | Generate all unique pairs of antennae indices, for the given number of
--   antennae, and block-size. E.g., for the block:
--   
--     ([0,1,2], [3,4,5])  |-->
--               [(0,3),(0,4),(0,5),(1,3),(1,4),(1,5),(2,3),(2,4),(2,5)] ;
--   
--   which is only the pairs between each set; i.e., a complete bipartite
--   graph between the two sets of nodes.
--   Inputs:
--    `a` -- number of antennae; and
--    `b` -- block-size.
blocks :: Z -> Z -> Block Z
blocks a b = go bs
  where
    bs = chunk b [0..a-1]
    go (_:[]) = []
    go (x:xs) = map (block x) xs ++ go xs

-- | For each (sub)block, generate all shared pairs. E.g., for the block:
--   
--     [0,1,2,3,4,5]  |-->  [(0,1),(0,2),(0,3),(0,4),(0,5),(1,2),(1,3),(1,4),
--                           (1,5),(2,3),(2,4),(2,5),(3,4),(3,5),(4,5)] ;
--   
--   i.e., the cliques (complete graphs) for the given node-sets; and these
--   pairwise computations need to be shared amongst all blocks with these
--   sets of indices.
shareds :: Z -> Z -> Pairs Z
shareds a b =
  let bs = concat $ blocks a b
      ps = pairs a
  in  ps \\ bs

------------------------------------------------------------------------------
-- | Generate the set of all antenna-index blocks. This function groups the
--   antennae into sets (of size `b`), and then computes all sets of (group)
--   pairs.
blockset :: Z -> Z -> BlockSet
blockset a b = go bs
  where
    bs = chunk b [0..a-1]
    go (_:[]) = []
    go (x:xs) = zipWith Pair (repeat x) xs ++ go xs


-- * Correlator-block index-set functions.
------------------------------------------------------------------------------
-- | Build the blocked set of antenna indices, for TART's correlators.
buildset :: Z -> Z -> Block Z
buildset a b =
  let (n,bz) = (length bz, blockset a b)
      (m,sz) = (length sz, shareds  a b)
      (c,wz) = (m `div` (n*2), blocks a b)
      (xz,s) = foldl (\(xs, s) b ->
                       let (x, s') = allocate c b s
                       in  (x:xs, s')) ([], sz) bz
  in  zipWith (++) wz (reverse xz)

------------------------------------------------------------------------------
-- | Traverse the list of shared pairwise interactions, and take some from
--   this list, to be added to the set of correlations, for the current block.
allocate :: Z -> BlockZ -> Pairs Z -> (Pairs Z, Pairs Z)
allocate n (Pair a b) ps =
  let xs = take n (common a ps) ++ take n (common b ps)
  in  (xs, ps \\ xs)

-- | For the given set of clique nodes, and a list of edges, filter the list
--   so that it only contains edges for the given set of nodes.
--   NOTE: Using `&&` vs. `||` should give the same result, as the given list
--     of pairs is a list of clique edges; therefore if one node belongs to an
--     edge, then so must the other.
common :: [Z] -> Pairs Z -> Pairs Z
common ns = filter (\(Pair i j) -> elem i ns && elem j ns)


-- * Antenna index-pair generator.
------------------------------------------------------------------------------
-- | Generates the correlator-pairs, for the given:
--    `a` -- antennae;
--    `b` -- blocksize; and,
--    `m` -- (time) multiplexing rate.
params :: Z -> Z -> Z -> Blocks Z
params a b m =
  let bs = buildset a b
      mx = [ ( 0, 1), ( 6, 7)   -- TODO: generate from parameters
           , ( 2, 3), (12,13)
           , ( 4, 5), (18,19)
           , ( 8, 9), (16,17)
           , (10,11), (20,21)
           , (14,15), (22,23)]
      ms = toMeans $ if a == 24 then mx else [ (i,i+1) | i <- [0,2..] ]
      -- ^ The last two index-pairs, of each block, are used for counting the
      --   number of ones, for four of the antennae.
      bz = let go ps     []  yz = (ps, yz)
               go ps (xs:xz) yz = let (ps', ys) = padEnds m ps xs
                                  in  second (ys:) $ go ps' xz yz
           in  snd $ go ms (chunk m <$> bs) []
  in  bz


-- * Emitters.
------------------------------------------------------------------------------
-- | Generate the Verilog parameters, for the correlator-blocks.
makeParams :: Z -> Z -> Z -> Shell Line
makeParams a b m = emitParams a b m $ params a b m

-- | Emit the generated parameters.
emitParams :: Z -> Z -> Z -> Blocks Z -> Shell Line
emitParams a b m bz =
  let -- ^ number of index-bits:
      ib = ceiling $ log (fromIntegral a) / log 2
      -- ^ number of parameter-bits/correlator:
      (n, l) = (length bz, length (head bz)) -- (#blocks, #correlators/block)
      -- ^ generate antenna-index parameter labels:
      lx = [ printf "PAIRS%02x_%02x = " i j | i <- [0..n-1], j <- [0..l-1] ]
      -- ^ generate antenna-index parameters:
      hx x = printf "{%d'h%x};" (ib*2*m) x :: String
      px = concatMap (map (hx . toBitfield ib)) bz
      pp = zipWith (++) (repeat "   parameter ") (zipWith (++) lx px)
  in  select $ unsafeTextToLine . pack <$> (emitHeader a b m:pp)

emitHeader :: Z -> Z -> Z -> String
emitHeader a b m =
  let hs = unlines [ "   //"
                   , "   // Generated using:"
                   , "   //   ./pairs.hs -a %d -b %d -m %d -o <file>"
                   , "   //"]
  in  printf (init hs) a b m


-- ** Emitter helper-functions.
------------------------------------------------------------------------------
-- | Generate the packed indices of the correlator pairs, where each index
--   has `b`-bits (therefore, each pair has  `2*b`-bits).
toBitfield :: Z -> Pairs Z -> Integer
toBitfield b =
  let a  = b+b
      go _     []  = 0
      go i (pq:qs) = case pq of
        Pair j k -> f j k
        Mean j k -> f j k
        where
          f p q = fromIntegral (q `shiftL` b .|. p) `shiftL` i .|. go (i+a) qs
  in  go 0


-- * Helper functions.
------------------------------------------------------------------------------
-- | Break a list into chunks of the given size.
chunk :: Z -> [a] -> [[a]]
chunk s xs
  | null zs   = [ys]
  | otherwise = ys:chunk s zs
  where
    (ys, zs)  = (take s xs, drop s xs)

------------------------------------------------------------------------------
-- | Pad the end of the (second) list with items from the first list, if it
--   has fewer than the given number of elements.
appendFrom :: Z -> [a] -> [a] -> ([a], [a])
appendFrom s zs xs
  | n  <  s   = (drop m zs, xs ++ take m zs)
  | otherwise = (zs, xs)
  where
    (n, m) = (length xs, s - n)

-- | For lists with length less than the specified minimum length, append
--   items from the given list of extra elements.
padEnds :: Z -> [a] -> [[a]] -> ([a], [[a]])
padEnds s = go []
  where
    go zz ps     []  = (ps, zz)
    go zz ps (ys:yz) = let (ps', zs) = appendFrom s ps ys
                       in  second (zs:) $ go zz ps' yz

------------------------------------------------------------------------------
toPairs :: [(a, a)] -> Pairs a
toPairs  = map (uncurry Pair)

fromPairs :: Pairs a -> [(a, a)]
fromPairs = map (\(Pair a b) -> (a, b))

toMeans :: [(a, a)] -> Means a
toMeans  = map (uncurry Mean)


-- * Program main.
------------------------------------------------------------------------------
-- | Parse command-line options.
parser :: Parser (Z, Z, Z, FilePath, Bool, Bool, Bool)
parser  = (,,,,,,) <$> optInt  "antennae"  'a' "The number of antennae"
                   <*> optInt  "blocksize" 'b' "The size of the antenna blocks"
                   <*> optInt  "multiplex" 'm' "The time-multiplexing ratio"
                   <*> optPath "outfile"   'o' "Output filename"
                   <*> switch  "verbose"   'v' "Show extra information"
                   <*> switch  "permute"   'p' "Generate permutation vector"
                   <*> switch  "hex"       'h' "Hexadecimal permutation vector"

------------------------------------------------------------------------------
-- | Display extra information when the `--verbose` option is used.
--   
--   TODO: Make all output compatible with Verilog, so make comments from the
--     lists of pairs.
--   TODO: Display the permutation vector.
verbose :: Z -> Z -> Z -> FilePath -> IO ()
verbose a b m o = do
  stdout $ select ["// ", "//  == Antenna pair generation ==",
                   "// ", "// Correlator block parameters:", "// "]
  stdout $ makeParams a b m
  stdout ""
  let f  = Set.fromList . uncurry (++) . unzip
      bs = f . fromPairs <$> buildset a b
      pz = params a b m
      ps = map (f . fromPairs) <$> pz
      ss = Set.fromList $ pairs a
  mapM_ (mapM_ print >=> const (putStrLn "")) pz

------------------------------------------------------------------------------
-- | Construct, and then emit the permutation vector to a (Numpy compatible)
--   ASCII file.
--   NOTE: To read using Numpy:
--     
--     > import numpy
--     > pp = numpy.loadtxt(<filename>, dtype='int')
--     
--     and then can be used to permute the visibilities, that have been read
--     back from the FPGA:
--     
--     > raw = tart.vis_read()
--     > viz = [raw[i] for i in pp] ;
--     
--     or, simply (due to Numpy's array functionality):
--     
--     > viz = tart.vis_read()[pp]
--     
makePermute :: Z -> Z -> Z -> Shell Line
makePermute a b =
  select . (:[]) . unsafeTextToLine . pack . intercalate " " . map show . permute a b

-- | Produces the permutation data in hexadecimal format, and suitable for
--   reading back with Verilog's `$readmemh` command.
makePermHex :: Z -> Z -> Z -> Shell Line
makePermHex a b =
  select . map (unsafeTextToLine . pack . printf "%03x") . permute a b

------------------------------------------------------------------------------
-- | Generate antenna-pair indices, for the correlators.
--   Default (Makefile) options:
--   
--     runhaskell script/pairs.hs --antennas=24 --blocksize=6 --multiplex=12 \
--       --outfile include/tart_pairs.v
--   
main :: IO ()
main  = do
  (a, b, m, o, v, p, h) <- options "" parser

  unless (a == 24 && b == 6 && m == 12) $ do
    putStrLn "Unsupported input parameters."

  when v $ verbose a b m o
  when p $ output o (makePermute a b m)
  when h $ output o (makePermHex a b m)
  when (not p && not h) $
    output o (makeParams a b m)
