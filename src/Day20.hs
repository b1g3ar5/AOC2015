{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor #-}

module Day20 where

import Utils


target :: Int
target = 36000000


factors :: Int -> [Int]
factors n = concat $ [1,n]:[[x, n `div` x] | x <- [2..rn], isFactor x]
  where
    rn = round $ sqrt $ fromIntegral n
    isFactor y = n `mod` y == 0


sumOfFactors50 :: Int -> Int
sumOfFactors50 n = sum $ filter (\f -> n `div` f < 50) $ factors n


primeFactors :: Int -> [(Int, Int)]
primeFactors n = (\g -> (head g, length g)) <$> group (primeFactors' n 2)
  where
    primeFactors' :: Int -> Int -> [Int]
    primeFactors' n f
      | f*f > n        = [n]
      | n `mod` f == 0 = f : primeFactors' (n `div` f) f
      | otherwise      = primeFactors' n (f + 1)


-- Uses the formula from prime factors
-- See: https://www.geeksforgeeks.org/sum-factors-number/
sumOfFactors :: Int -> Int
sumOfFactors n = product $ (\(x, a) -> (x^(a+1) - 1) `div` (x-1)) <$> pfs
  where
    pfs = primeFactors n


findTarget1 :: Int
findTarget1 = go 750000
  where
    go p
      | 10 * sumOfFactors p >= target = p
      | otherwise = go (p+1)


findTarget2 :: Int
findTarget2 = go 750000
  where
    go p
      | 11 * sumOfFactors50 p >= target = p
      | otherwise = go (p+1)


day20 :: IO ()
day20 = do
  ss <- getLines 20
  putStrLn $ "Day20: part1: " ++ show findTarget1
  putStrLn $ "Day20: part2: " ++ show findTarget2

  return ()
