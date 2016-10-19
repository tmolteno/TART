

-- * from `pairs.hs`.
------------------------------------------------------------------------------
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

-- | Repeat elements until the list is the given length, if possible.
adjust :: Z -> [a] -> [a]
adjust s xs | null xs   = []
            | n  <  s   = adjust s $ xs ++ take (s-n) xs
            | otherwise = xs
  where
    n = length xs

-- | Choose blocks to put the shared pair-wise interactions.
choices :: [([Z], [Z])] -> [(Z, Z)] -> [[(Z, Z)]]
choices bz sz =
  let go (a, b) = let bs = a ++ b
                  in  filter (\(i, j) -> elem i bs || elem j bs) sz
  in  map go bz

{-- }
-- newtype Pairs a = Pairs { unPairs :: [Pair a] }
--                 deriving (Eq, Read, Show, Functor) -- , Foldable, Traversable)

-- newtype Block a = Block { unBlock :: [Pairs a] }
--                 deriving (Eq, Read, Show, Functor) -- , Foldable, Traversable)

data Pairs a = Block [Pairs a]
             | Pairs [Pairs a]
             | Pair  a a
             | Sums  a a
             deriving (Eq, Read, Show, Functor, Foldable, Traversable)
--}

-- type Blocks a = [Pairs a]
-- type BlockSet = Pairs [Z]

{-- }
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
