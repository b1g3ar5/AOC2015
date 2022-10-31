module Day3(
  day3
  ) where

import Data.Map hiding (filter)
import Data.List hiding (union, singleton)
import Data.List.Split

day3 :: IO ()
day3 = do
  bs <- getData
  let p = parsePath bs
  putStrLn $ "Day3: Part1: " ++ show (size p)
  let twos = transpose $ chunksOf 2 bs
  let santa = twos!!0
  let robot = twos!!1
  let santaHouses = parsePath santa
  let robotHouses = parsePath robot
  let allHouses = union santaHouses robotHouses
  putStrLn $ "Day3: Part2: " ++ show (size allHouses)
  return ()


getData :: IO [Direction]
getData = do
  txt <- readFile "Data/Day3.in"
  return $ parse <$> filter (\c -> c=='<' || c == '>' || c== '^' || c== 'v') txt


data Direction = N | S | E | W deriving (Show, Eq)

type Location = (Int, Int)

step :: Location -> Direction -> Location
step (x, y) N = (x, y+1)
step (x, y) S = (x, y-1)
step (x, y) E = (x+1, y)
step (x, y) W = (x-1, y)

parse :: Char -> Direction
parse '^' = N
parse 'v' = S
parse '>' = E
parse '<' = W
parse c = error $ "Trying to parse: " ++ [c] ++ "."

type Houses = Map Location Int

parsePath :: [Direction] -> Houses
parsePath directions = go (singleton (0,0) 1) (0, 0) directions
  where
    go m l [] = m
    go m l (d:ds) = go (insertWith (+) newLocation 1 m) newLocation ds
      where
        newLocation = step l d
