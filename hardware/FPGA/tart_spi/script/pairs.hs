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
-- NOTE:
--  + requires `text` and `turtle` to be installed:
--      > cabal install text turtle
--      > make pairs
-- 
-- Changelog:
--  + 16/06/2016  --  initial file;
--  + 15/10/2016  --  permutation vector support added;
-- 
-- FIXME:
-- 
-- TODO:
--  + only inputs `a == 24, b == 6, m == 12` are correctly supported.
-- 
------------------------------------------------------------------------------

module Main where

import Prelude hiding (FilePath)
import Control.Arrow
import Data.Bits
import Data.List
import Data.Maybe
import Data.Tuple
import Data.Text (pack, unpack)
import Text.Printf
import Turtle hiding (printf)

import qualified Data.Set as Set
import qualified Data.Map as Map


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
  in  zipWith (++) wz (reverse xz)

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

------------------------------------------------------------------------------
-- | Generate the Verilog parameters, for the correlator-blocks.
makeParams :: Z -> Z -> Z -> Shell Text
makeParams a b m = showParams a b m $ params a b m

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
verbose :: Z -> Z -> Z -> FilePath -> IO ()
verbose a b m o = do
  stdout " == Antenna pair generation =="
  stdout "\nCorrelator block parameters:"
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
  mapM_ (print . Set.size . f . concat) pz
--   print ss
--   print $ Set.size ss
--   print $ foldr Set.insert Set.empty (concat (concat pz))
--   print $ foldr Set.delete ss (concat (concat pz))
  mapM_ (mapM_ print >=> const (putStrLn "")) pz

------------------------------------------------------------------------------
-- | Generate the permutation vector, to reorder the visibilities into the
--   standard "triangular" ordering.
--   
--   TODO:
permute :: Z -> Z -> Z -> IO ()
permute a b m = do
  let pz = params a b m
      ri = [0,(2*a)..]
      ps = indexCalc a . concat <$> pz
      n  = pred a*div a 2
      s  = n + div a 2
      t  = length (head ps) - 2
      ms = zipWith (indexMeans a) ri $ concat <$> pz
      qs = zipWith indexPairs ri ps
      rs = [(i*pred a + j, n + i*2 + j) | i <- [0..b-1], j <- [0,1]]
      pm = Map.fromList $ map swap $ concat qs ++ rs
--   print n
--   mapM_ print qs
  print pm

{-- }
permute a b m = do
  ar <- Vec.new (a*a)
  let ix = [
  let pz = params a b m
      ri = [0,(2*a)..]
      ps = indexCalc a . concat <$> pz
      n  = pred a*div a 2
      s  = n + div a 2
      t  = length (head ps) - 2
      ms = zipWith (indexMeans a) ri $ concat <$> pz
      qs = zipWith indexPairs ri ps
      rs = [(i*pred a + j, n + i*2 + j) | i <- [0..b-1], j <- [0,1]]
      pm = Map.fromList $ map swap $ concat qs ++ rs
--   print n
--   mapM_ print qs
  print pm
--}

-- | Index each of the pairs.
--   NOTE: Assumes that the last two pairs are indices that were used for
--     computing antenna signal means.
indexPairs :: Z -> [a] -> [(Z, a)]
indexPairs from ps =
  let ps' = zip [from..] ps
  in  init $ init ps'

indexMeans :: Z -> Z -> [a] -> [(Z, a)]
indexMeans stride from ps =
  let ps' = zip [from..] ps
  in  drop (stride - 2) ps'

-- | The "triangular" indices have `stride-1` entries in row zero, and then
--   one less for each subsequent row.
indexCalc :: Z -> [(Z, Z)] -> [Z]
indexCalc stride =
  let f (i, j) = ((2*stride - i - 3) * i) `div` 2 + j - 1
  in  map f

------------------------------------------------------------------------------
-- | Generate antenna-pair indices, for the correlators.
--   Default (Makefile) options:
--     runhaskell script/pairs.hs --antennas=24 --blocksize=6 --multiplex=12 \
--       --outfile include/tart_pairs.v
--   
main :: IO ()
main  = do
  (a, b, m, o, v, p) <- options "" parser

  unless (a == 24 && b == 6 && m == 12) $ do
    putStrLn "Unsupported input parameters."

  when v $ verbose a b m o
  when p $ permute a b m
  output o $ makeParams a b m
