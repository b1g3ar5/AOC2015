{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Day18 where

import Utils
import Data.Bool (Bool)
import qualified Data.Map.Strict as M


type Grid = Map Coord Int

maxCoord :: Int
maxCoord = 99


neighbours :: Coord -> [Coord]
neighbours c = filter (\(x,y) -> x>=0 && y>=0 && x<=maxCoord && y<=maxCoord) $ neighbours8 c


parseGrid :: [String] -> Map Coord Int
parseGrid xss = M.fromList $ concat $ (\(y, xs) -> (\(x,c) -> ((x,y), if c=='#' then 1 else 0)) <$> zip [0..] xs) <$> zip [0..] xss


type Rule = Grid -> Coord -> Int


rule1 :: Rule
rule1 g c 
  | g M.! c == 1 = if count==2 || count==3 then 1 else 0
  | otherwise = if count==3 then 1 else 0
  where
    count = sum $ (g M.!) <$> neighbours c

rule2 :: Rule
rule2 g c 
  | c == (0,0) = 1
  | c == (0,99) = 1
  | c == (99,0) = 1
  | c == (99,99) = 1
  | g M.! c == 1 = if count==2 || count==3 then 1 else 0
  | otherwise = if count==3 then 1 else 0
  where
    count = sum $ (g M.!) <$> neighbours c


update :: Rule -> Grid -> Grid
update rule g = M.fromList $ (\k -> (k, rule g k) ) <$> ks
  where
    ks = M.keys g


day18 :: IO ()
day18 = do
  ss <- getLines 18
  let g :: Grid
      g = parseGrid ss
  putStrLn $ "Day18: part1: " ++ show (sum $ iterate (update rule1) g !! 100)
  putStrLn $ "Day18: part2: " ++ show (sum $ iterate (update rule2) g !! 100)

  return ()
