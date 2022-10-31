{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Day14 where

import Utils


data Reindeer = Reindeer { speed :: Int, flytime :: Int, restTime :: Int} deriving (Eq, Show)

parseLine :: String -> Reindeer
parseLine s = Reindeer (read $ bs!!3) (read $ bs !!6) (read $ bs !!13)
  where
    bs = words s


distance :: Int -> Reindeer -> Int
distance time (Reindeer v ft rt) = v * (ft * q + min ft r)
  where
    (q,r) = quotRem time (ft + rt)

    
points :: [Reindeer] -> Int
points rs = maximum $ sum <$> transpose (score . (\d -> distance d <$> rs) <$> [1..2503])


score :: [Int] -> [Int]
score xs = (\x -> if x==mx then 1 else 0) <$> xs
  where
    mx = maximum xs


day14 :: IO ()
day14 = do
  ss <- getLines 14
  let deer = parseLine <$> ss
      t = 2503
  putStrLn $ "Day14: part1: " ++ show (maximum $ distance t <$> deer)
  putStrLn $ "Day14: part1: " ++ show (points deer)

  return ()
