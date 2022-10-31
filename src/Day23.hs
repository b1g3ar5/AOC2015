{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Day23 where

import Utils (getLines, Map)
import qualified Data.Map as M


data Ins = Hlf Char| Tpl Char | Inc Char | Jmp Int | Jie Char Int | Jio Char Int deriving (Show, Eq)


parseIns :: String -> Ins
parseIns s
  | head ws == "hlf" = Hlf $ head $ ws!!1 
  | head ws == "tpl" = Tpl $ head $ ws!!1 
  | head ws == "inc" = Inc $ head $ ws!!1  
  | head ws == "jmp" = Jmp $ read $ filter (/='+') $ ws!!1  
  | head ws == "jie" = Jie (head $ ws!!1) (read $ filter (/='+') $ ws!!2)
  | head ws == "jio" = Jio (head $ ws!!1) (read $ filter (/='+') $ ws!!2)
  | otherwise = undefined
  where
    ws = words s


run :: Int -> (Map Char Int, [Ins]) -> Map Char Int
run pos (mp, is)
  | pos >= length is = mp
  | otherwise = 
    case i of 
      Hlf x -> run (pos+1) (M.adjust (`div` 2) x mp, is)
      Tpl x -> run (pos+1) (M.adjust (* 3) x mp, is)
      Inc x -> run (pos+1) (M.adjust (+ 1) x mp, is)
      Jmp n -> run (pos+n) (mp, is)
      Jie x n -> run (pos+ if even (mp M.! x) then n else 1) (mp, is)
      Jio x n -> run (pos+ if (mp M.! x) == 1 then n else 1) (mp, is)
  where
    i = is !! pos


day23 :: IO ()
day23 = do
  ss <- getLines 23
  let ins = parseIns <$> ss
  putStrLn $ "Day23: part1: " ++ show (run 0 (M.fromList [('a', 0), ('b', 0)], ins) M.! 'b')
  putStrLn $ "Day23: part2: " ++ show (run 0 (M.fromList [('a', 1), ('b', 0)], ins) M.! 'b')

  return ()
