#!/usr/bin/env runhaskell
{-# LANGUAGE OverloadedStrings, TypeOperators #-}

module Main where

import Prelude hiding (FilePath)
import Data.Bits
import Data.List
import Data.Maybe
import Data.Bool
import Data.Text (pack, unpack)
import Text.Printf
import Turtle hiding (printf)


type Z = Int

mux2 :: Bool -> Bool -> Bool -> Bool
mux2 False _ a = a
mux2 True  b _ = b

mux4 :: Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool
mux4 s1 s0 d c b a = mux2 s1 (mux2 s0 d c) (mux2 s0 b a)

lut6 :: (Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool) -> String
lut6 f = lut5 (f True) ++ lut5 (f False)

lut5 :: (Bool -> Bool -> Bool -> Bool -> Bool -> Bool) -> String
lut5 f = lut4 (f True) ++ lut4 (f False)

lut4 :: (Bool -> Bool -> Bool -> Bool -> Bool) -> String
lut4 f = lut3 (f True) ++ lut3 (f False)

lut3 :: (Bool -> Bool -> Bool -> Bool) -> String
lut3 f = lut2 (f True) ++ lut2 (f False)

lut2 :: (Bool -> Bool -> Bool) -> String
lut2 f = lut1 (f True) ++ lut1 (f False)

lut1 :: (Bool -> Bool) -> String
lut1 f = bit True:bit False:[]
  where
    bit = bool '0' '1' . f


main = do
  printf "8'b%s\n" $ lut3 mux2
  printf "64'b%s\n" $ lut6 mux4
