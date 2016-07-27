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
import Data.Bits
import Data.List
import Data.Maybe
import Data.Text (pack, unpack)
import Text.Printf
import Turtle hiding (printf)


type Z = Int


floorDSP :: LOC -> Z -> String
floorDSP l w = ""


------------------------------------------------------------------------------
-- | Parse command-line options.
parser :: Parser (Z, Z, Z, FilePath)
parser  = (,,,) <$> optInt  "antennas"  'a' "The number of antennae"
                <*> optInt  "blocksize" 'b' "The size of the antenna blocks"
                <*> optInt  "multiplex" 'm' "The time-multiplexing ratio"
                <*> optPath "outfile  " 'o' "Output filename"

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
