{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Day13 where

import Utils
import qualified Data.Map.Strict as M
import qualified TotalMap as T


type Name = String
type Edge = ((Name, Name), Int)
type EdgeMap = T.TMap (Name, Name) Int


parseLine :: String -> Edge
parseLine s = ((head bs, init $ bs!!10), v * read (bs!!3))
  where
    bs = words s
    v = if bs!!2 == "gain" then 1 else -1


-- Add the first element on to the end (it's a round table)
amend :: [a] -> [a]
amend [] = []
amend (x:xs) = x:xs ++ [x]

score :: EdgeMap -> [Name] -> Int
score em = go 0
  where
    go acc [] = acc
    go acc [n] = acc
    go acc (n:m:ns) = go (acc + em T.! (n, m) + em T.! (m, n)) (m:ns)


day13 :: IO ()
day13 = do
  ls <- getLines 13
  let em = T.TMap 0 $ M.fromList $ parseLine <$> ls
      names = nub $ head . words <$> ls
      scores1 = score em . amend <$> permutations names
      scores2 = score em . amend <$> permutations ("X":names)
  putStrLn $ "Day13: part1: " ++ show (maximum scores1)
  putStrLn $ "Day13: part2: " ++ show (maximum scores2)

  return ()

