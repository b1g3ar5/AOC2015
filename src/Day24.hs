{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Day24 where

import Utils ( getLines, partition, minimumBy, sortBy, comparing )
import Data.Ord
import Data.List
import Control.Monad


data Group = Group { weight :: Int, entanglement :: Integer, packages :: [Int]} deriving (Show, Eq)


makeGroups :: Int -> [Int] -> [[Group]]
makeGroups n weights = go (replicate n $ Group 0 1 []) weights
  where
    targetWeight = sum weights `div` n
    go :: [Group] -> [Int] -> [[Group]]
    go acc [] = return $ sortOn entanglement acc
    go acc (x:xs) = do
      gp <- availableGroups
      let newGps = (\(i, g@(Group wgt emt ps)) -> 
                        if i==gp then 
                          Group (wgt+x) (fromIntegral x*emt) (x:ps) 
                        else 
                          g
                    ) <$> zip [0..] acc
      go newGps xs
      where 
        (availableGroups, _) = partition (\ix -> weight (acc!!ix) + x <= targetWeight) [0..(n-1)]


findBest :: [[Group]] -> [Group]
findBest gs = minimumBy (comparing minEntangelment) minGs
  where
    minL = minimum $ minLength <$> gs
    minGs = filter (\g -> minLength g == minL) gs 

minLength :: [Group] -> Int
minLength gs = minimum $ length . packages <$> gs


minEntangelment :: [Group] -> Integer
minEntangelment gs = minimum $ entanglement <$> minGs
  where
    minL = minLength gs
    minGs = filter (\g -> length (packages g) == minL) gs 


day24 :: IO ()
day24 = do
  ss <- getLines 24
  let weights :: [Int]
      weights = reverse $ read <$> ss


  putStrLn $ "Day24: part1: " ++ show (entanglement $ head $ findBest $ take 10 $ makeGroups 3 weights)
  --putStrLn $ "Day24: part1: " ++ show (entanglement $ head $ findBest $ take 10 $ makeGroups 4 weights)
  putStrLn $ "Day24: part2: " ++ show (product $ head $ head $ groupBy (\xs ys -> product xs > product ys) (sortOn product $ take 1000 (get5 weights)))
  
  return ()


-- Simple method - just get the group of 5 which has lowest entanglement...
get5 :: [Int] -> [[Int]]
get5 ns = do
  x1 <- ns
  x2 <- ns
  x3 <- ns
  x4 <- ns
  x5 <- ns
  guard $ x1 /= x2
  guard $ x1 /= x3
  guard $ x1 /= x4
  guard $ x1 /= x5
  guard $ x2 /= x3
  guard $ x2 /= x4
  guard $ x2 /= x5
  guard $ x3 /= x4
  guard $ x3 /= x5
  guard $ x4 /= x5
  guard $ (x1+x2+x3+x4+x5) == (sum ns `div` 4)
  return [x1,x2,x3,x4,x5]