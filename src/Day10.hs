{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Day10 where

import Utils (group)


lookAndSay :: String -> String
lookAndSay s = concat $ (\g -> show (length g) ++ [head g]) <$> gs
  where
    gs = group s


day10 :: IO ()
day10 = do
  let start = "1321131112"
  putStrLn $ "Day10: part1: " ++ show (length $ iterate lookAndSay start !! 40)
  putStrLn $ "Day10: part2: " ++ show (length $ iterate lookAndSay start !! 50)

  return ()
