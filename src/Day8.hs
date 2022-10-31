{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Day8 where

import Utils (getLines)
import Data.Char (chr)


decode :: String -> String 
decode = go []
  where
    go acc [] = acc
    go acc [c] = acc ++ [c]
    go acc (c:d:cs) = 
      if c == '\\' then 
        case d of 
          '\\' -> go (acc ++ ['\\']) cs
          '"' -> go (acc ++ ['"']) cs
          'x' -> go (acc ++ [chr $ read $ take 2 cs]) (drop 2 cs)
          _  -> error $ "Shouldn't get here, d = " ++ [d]
      else
        go (acc ++ [c]) (d:cs)


encode :: String -> String 
encode = go []
  where
    go acc [] = acc
    go acc (c:cs) = 
      case c of 
        '\\' -> go (acc ++ ['\\', '\\']) cs
        '"' -> go (acc ++ ['\\', '"']) cs
        _  -> go (acc ++ [c]) cs


totalLength :: [String] -> Int
totalLength ss = sum $ length <$> ss

day8 :: IO ()
day8 = do
  ls <- getLines 8

  putStrLn $ "Day8: part1: " ++ show (2 * length ls + totalLength ls - totalLength (decode <$> ls ))
  putStrLn $ "Day8: part2: " ++ show (2 * length ls + totalLength (encode <$> ls) - totalLength ls )

  return ()
