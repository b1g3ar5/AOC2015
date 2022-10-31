module Lib
    ( someFunc
    ) where

import Day1
import Day2
import Day3
import Day4
import Day5
import Day6
import Day7
import Day8
import Day9
import Day10
import Day11
import Day12
import Day13
import Day14
import Day15
import Day16
import Day17
import Day18
import Day19
import Day20
import Day21
import Day22
import Day22g
import Day23
import Day24
import Day25

someFunc :: IO ()
someFunc = do
  day1
  day2
  day3
  day4 -- This is a bit slow - the md5 hash function..
  day5
  day6
  day7
  day8
  day9
  day10
  day11
  day12
  day13
  day14
  day15
  day16
  day17
  day18
  day19
  day20
  day21
  day22
  day22g
  day23
  day24
  day25


makeFiles :: IO ()
makeFiles = do sequence_ (makeFile <$> [8..25])

makeFile :: Int -> IO ()
makeFile n = do 
  writeFile ("./Day" ++ show n ++ ".hs") (proforma n)


proforma :: Int -> String
proforma n = unlines [
  "{-# LANGUAGE TypeSynonymInstances #-}"
  , "{-# LANGUAGE FlexibleInstances #-}"
  , ""
  , "module Day" ++ show n ++ " where"
  , ""
  , "import Utils"
  , ""
  , "day" ++ show n ++ " :: IO ()"
  , "day" ++ show n ++ " = do"
  , "  ss <- getLines " ++ show n ++ ""
  , "  putStrLn $ \"Day" ++ show n ++ ": part1: \" ++ show \"\""
  , "  putStrLn $ \"Day" ++ show n ++ ": part1: \" ++ show \"\""
  , ""
  , "  return ()"
  ]  
