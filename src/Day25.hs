{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Day25 where

import Utils

next :: Int -> Int
next x = x*252533 `rem` 33554393

triangle 1 = 1
triangle n = n * (n + 1) `div` 2

{-
We need to find out how many codes to get to y=2981, x=3075
Well there are 3075 codes to get to back to x=6054, y=1
The code at x=6045 is the sum_{i=1}^{i=6045}(i) ie the 6054th triangle number

So we need the (3075 + triangle 6045)th code
The index of this code is (3074 + triangle 6045)

-}

day25 :: IO ()
day25 = do
  let x, y :: Int
      y = 2981
      x = 3075
      code = 20151125
      codes = iterate next code
      ix = 3074 + triangle 6054
  putStrLn $ "Day25: part1: " ++ show ix
  putStrLn $ "Day25: part1: " ++ show (take 20 codes)
  putStrLn $ "Day25: part1: " ++ show (codes !! ix)

  -- 28836990 too high
  return ()
