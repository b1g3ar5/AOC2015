{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Day9 where

import Utils


type Name = String 
type Edge = ((Name, Name), Int)


parseLine :: String -> Edge
parseLine s = ((head ps, ps!!2), read $ ps!!4)
  where
    ps = words s


distLookup :: Name -> Name -> [Edge] -> Int
distLookup a b [] = error "Couldn't find the distance"
distLookup a b (((s1, s2), d):es) 
  | s1 == a && s2 == b = d
  | s1 == b && s2 == a = d
  | otherwise = distLookup a b es


dist :: [Edge] -> [Name] -> Int
dist es = go 0
  where
    go :: Int -> [Name] -> Int
    go acc [] = acc
    go acc [s] = acc
    go acc (x:y:xs) = go (acc + d) (y:xs)
      where
        d = distLookup x y es


day9 :: IO ()
day9 = do
  ss <- getLines 9
  let edges = parseLine <$> ss
      stars = (fst . fst . head $ edges) : (snd . fst <$> take 7 edges)
      ps = permutations stars
      ds = dist edges <$> ps
  putStrLn $ "Day9: part1: " ++ show (minimum ds)
  putStrLn $ "Day9: part2: " ++ show (maximum ds)

  return ()
