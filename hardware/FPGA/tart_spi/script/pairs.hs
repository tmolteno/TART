#!/usr/bin/env runhaskell
{-# LANGUAGE OverloadedStrings, TypeOperators #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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
import Data.Bits
import Data.List
import Data.Maybe
import Data.Tuple
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
             deriving (Eq, Read, Show, Functor, Foldable, Traversable)

-- ^ convenience aliases:
type Z = Int

type Pairs  a = [Pair  a]
type Means  a = [Pair  a]
type Block  a = [Pairs a]
type Blocks a = [Block a]

type BlockZ   =  Pair [Z]
type BlockSet = [Pair [Z]]


-- * Nouveaux antenna-pair generators.
------------------------------------------------------------------------------
mkPairs :: Z -> Pairs Z
mkPairs  = toPairs . pairs

mkBlocks :: Z -> Z -> Block Z
mkBlocks a = map toPairs . blocks a

mkBlockset :: Z -> Z -> BlockSet
mkBlockset a = toPairs . blockset a

mkShareds :: Z -> Z -> Pairs Z
mkShareds a = toPairs . shareds a

getPairs :: [Z] -> Pairs Z -> Pairs Z
getPairs ns = toPairs . common ns . fromPairs

allocPairs :: Z -> BlockZ -> Pairs Z -> (Pairs Z, Pairs Z)
allocPairs n (Pair a b) =
  fromPairs >>> allocate n (a, b) >>> toPairs *** toPairs


-- ** Nouveaux helper-functions.
------------------------------------------------------------------------------
toPairs :: [(a, a)] -> Pairs a
toPairs  = map (uncurry Pair)

fromPairs :: Pairs a -> [(a, a)]
fromPairs = map (\(Pair a b) -> (a, b))

toMeans :: [(a, a)] -> Means a
toMeans  = map (uncurry Mean)


-- * Nouveaux antenna-pair set generators.
------------------------------------------------------------------------------
-- | Build the blocked set of antenna indices, for TART's correlators.
buildSet :: Z -> Z -> Block Z
buildSet a b =
  let (n,bz) = (length bz, mkBlockset a b)
      (m,sz) = (length sz, mkShareds  a b)
      (c,wz) = (m `div` (n*2), mkBlocks a b)
      (xz,s) = foldl (\(xs, s) b ->
                       let (x, s') = allocPairs c b s
                       in  (x:xs, s')) ([], sz) bz
  in  zipWith (++) wz (reverse xz)

-- | Generates the correlator-pairs, for the given:
--    `a` -- antennae;
--    `b` -- blocksize; and,
--    `m` -- (time) multiplexing rate.
mkParams :: Z -> Z -> Z -> Blocks Z -- [[[(Z, Z)]]]
mkParams a b m =
  let bs = buildSet a b
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


-- ** Nouveaux emitter-functions.
------------------------------------------------------------------------------
-- | Emit the generated parameters.
emitParams :: Z -> Z -> Z -> Blocks Z -> Shell Text
emitParams a b m bz =
  let -- ^ number of index-bits:
      ib = ceiling $ log (fromIntegral a) / log 2
      -- ^ number of parameter-bits/correlator:
      (n, l) = (length bz, length (head bz)) -- (#blocks, #correlators/block)
      -- ^ generate antenna-index parameter labels:
      lx = [ printf "PAIRS%02x_%02x = " i j | i <- [0..n-1], j <- [0..l-1] ]
      -- ^ generate antenna-index parameters:
      hx x = printf "{%d'h%x};" (ib*2*m) x :: String
      px = concatMap (map (hx . mkBitfield ib)) bz
      pp = zipWith (++) (repeat "   parameter ") (zipWith (++) lx px)
  in  select $ pack <$> (showHeader a b m:pp)

mkBitfield :: Z -> Pairs Z -> Integer
mkBitfield b = go 0
  where
    go _        []  = 0
    go i (Pair p q:qs) = x .|. go (i+a) qs
      where
        x = fromIntegral (q `shiftL` b .|. p) `shiftL` i
    a  = b+b


-- ** Permutation vector functionality.
------------------------------------------------------------------------------
-- | Generate the permutation vector, to reorder the visibilities into the
--   standard "triangular" ordering.
permute :: Z -> Z -> Z -> [Z]
permute a b m =
  let pz = mkParams a b m
      ps = concat $ concat <$> pz
      pm = pairIndex a ps
      go (Pair p q:ps) = p:q:go ps
      go (Mean p q:ps) = p:q:go ps
      go           []  = []
  in  go pm

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
pairs :: Z -> [(Z, Z)]
pairs n = go 0
  where
    go i | i  <  n   = repeat i `zip` [i+1..n-1] ++ go (i+1)
         | otherwise = []

-- | Generate all unique pairwise interactions for the two blocks of antenna
--   indices; i.e., computes the (complete) bipartite graph for the two sets
--   of nodes.
block :: [Z] -> [Z] -> [(Z, Z)]
block (m:ms) ns = repeat m `zip` ns ++ block ms ns
block    []   _ = []

------------------------------------------------------------------------------
-- | Generate the set of all antenna-index blocks. This function groups the
--   antennae into sets (of size `b`), and then computes all sets of (group)
--   pairs.
blockset :: Z -> Z -> [([Z], [Z])]
blockset a b = go bs
  where
    bs = chunk b [0..a-1]
    go (_:[]) = []
    go (x:xs) = repeat x `zip` xs ++ go xs

------------------------------------------------------------------------------
-- | Generate all unique pairs of antennae indices, for the given number of
--   antennae, and block-size. E.g., for the block:
--   
--     ([0,1,2], [3,4,5])  |-->
--               [(0,3),(0,4),(0,5),(1,3),(1,4),(1,5),(2,3),(2,4),(2,5)] ;
--   
--   which is only the pairs between each set; i.e., a complete bipartite
--   graph between the two sets of nodes.
blocks :: Z -> Z -> [[(Z, Z)]]
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
shareds :: Z -> Z -> [(Z, Z)]
shareds a b =
  let bs = concat $ blocks a b
      ps = pairs a
  in  ps \\ bs


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
  in  zipWith (++) wz (reverse xz)

------------------------------------------------------------------------------
-- | Traverse the list of shared pairwise interactions, and take some from
--   this list, to be added to the set of correlations, for the current block.
--   
--   TODO: This code may not be robust? It relies on the order in which the
allocate :: Z -> ([Z], [Z]) -> [(Z, Z)] -> ([(Z, Z)], [(Z, Z)])
allocate n (a, b) sz =
  let xs = take n (common a sz) ++ take n (common b sz)
  in  (xs, sz \\ xs)

-- | For the given set of clique nodes, and a list of edges, filter the list
--   so that it only contains edges for the given set of nodes.
--   NOTE: Using `&&` vs. `||` should give the same result, as the given list
--     of pairs is a list of clique edges; therefore if one node belongs to an
--     edge, then so must the other.
common :: [Z] -> [(Z, Z)] -> [(Z, Z)]
common xs = filter (\(i, j) -> elem i xs && elem j xs)


-- * Antenna index-pair generator.
------------------------------------------------------------------------------
-- | Generates the correlator-pairs, for the given:
--    `a` -- antennae;
--    `b` -- blocksize; and,
--    `m` -- (time) multiplexing rate.
params :: Z -> Z -> Z -> [[[(Z, Z)]]]
params a b m =
  let bs = buildset a b
--       ms = [ (i,i+1) | i <- [0,2..] ]
      mx = [ ( 0, 1), ( 6, 7)   -- TODO: generate from parameters
           , ( 2, 3), (12,13)
           , ( 4, 5), (18,19)
           , ( 8, 9), (16,17)
           , (10,11), (20,21)
           , (14,15), (22,23)]
--       ms = if a == 24 then map swap mx else [ (i,i+1) | i <- [0,2..] ]
      ms = if a == 24 then mx else [ (i,i+1) | i <- [0,2..] ]
--       ms = reverse mx
--       ms = drop 6 mx ++ take 6 mx
--       ms = repeat (24,24)
      -- ^ The last two index-pairs, of each block, are used for counting the
      --   number of ones, for four of the antennae.
      bz = let go ps     []  yz = (ps, yz)
               go ps (xs:xz) yz = let (ps', ys) = padEnds m ps xs
                                  in  second (ys:) $ go ps' xz yz
           in  snd $ go ms (chunk m <$> bs) []
--       bz = map (fmap (adjust m) . chunk m) bs
  in  bz


-- * Emitters.
------------------------------------------------------------------------------
-- | Generate the Verilog parameters, for the correlator-blocks.
makeParams :: Z -> Z -> Z -> Shell Text
makeParams a b m = showParams a b m $ params a b m

-- | Emit the generated parameters.
showParams :: Z -> Z -> Z -> [[[(Z, Z)]]] -> Shell Text
showParams a b m bz =
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
  in  select $ pack <$> (showHeader a b m:pp)

showHeader :: Z -> Z -> Z -> String
showHeader a b m =
  let hs = unlines [ "   //"
                   , "   // Generated using:"
                   , "   //   ./pairs.hs -a %d -b %d -m %d -o <file>"
                   , "   //"]
  in  printf (init hs) a b m


-- ** Emitter helper-functions.
------------------------------------------------------------------------------
toBitfield :: Z -> [(Z, Z)] -> Integer
toBitfield b = go 0
  where
    go _        []  = 0
    go i ((p,q):qs) = x .|. go (i+a) qs
      where
        x = fromIntegral (q `shiftL` b .|. p) `shiftL` i
    a  = b+b


-- * Helper functions.
------------------------------------------------------------------------------
-- | Break a list into chunks of the given size.
chunk :: Z -> [a] -> [[a]]
chunk s xs
  | null zs   = [ys]
  | otherwise = ys:chunk s zs
  where
    (ys, zs) = (take s xs, drop s xs)

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


-- * Program main.
------------------------------------------------------------------------------
-- | Parse command-line options.
parser :: Parser (Z, Z, Z, FilePath, Bool, Bool)
parser  = (,,,,,) <$> optInt  "antennae"  'a' "The number of antennae"
                  <*> optInt  "blocksize" 'b' "The size of the antenna blocks"
                  <*> optInt  "multiplex" 'm' "The time-multiplexing ratio"
                  <*> optPath "outfile"   'o' "Output filename"
                  <*> switch  "verbose"   'v' "Show extra information"
                  <*> switch  "permute"   'p' "Generate permutation vector"

------------------------------------------------------------------------------
-- | Display extra information when the `--verbose` option is used.
--   
--   TODO: Make all output compatible with Verilog, so make comments from the
--     lists of pairs.
--   TODO: Display the permutation vector.
verbose :: Z -> Z -> Z -> FilePath -> IO ()
verbose a b m o = do
  stdout "// \n//  == Antenna pair generation =="
  stdout "// \n// Correlator block parameters:\n// "
  stdout $ makeParams a b m
  stdout ""
  let f  = Set.fromList . uncurry (++) . unzip
      bs = f <$> buildset a b
      pz = params a b m
      ps = map f <$> pz
      ss = Set.fromList $ pairs a
--   mapM_ print bs
--   mapM_ print ps
--   mapM_ (print . Set.size) bs
--   mapM_ (print . map Set.size) ps
--   mapM_ (print . Set.size . f . concat) pz
--   print ss
--   print $ Set.size ss
--   print $ foldr Set.insert Set.empty (concat (concat pz))
--   print $ foldr Set.delete ss (concat (concat pz))
  mapM_ (mapM_ print >=> const (putStrLn "")) pz

------------------------------------------------------------------------------
-- | Construct, and then emit the permutation vector to a (Numpy compatible)
--   ASCII file.
--   
--   TODO:
makePermute :: Z -> Z -> Z -> Shell Text
makePermute a b m = do
  let ps = permute a b m
  select $ (:[]) $ pack $ intercalate " " $ map show ps

------------------------------------------------------------------------------
-- | Generate antenna-pair indices, for the correlators.
--   Default (Makefile) options:
--   
--     runhaskell script/pairs.hs --antennas=24 --blocksize=6 --multiplex=12 \
--       --outfile include/tart_pairs.v
--   
main :: IO ()
main  = do
  (a, b, m, o, v, p) <- options "" parser

  unless (a == 24 && b == 6 && m == 12) $ do
    putStrLn "Unsupported input parameters."

  when v $ verbose a b m o
  if p
    then output o $ makePermute a b m
    else output o $ makeParams  a b m
