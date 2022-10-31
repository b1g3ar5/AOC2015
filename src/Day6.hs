{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Day6 where

import Data.List.Split ( splitOn )
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Set ( Set, union, fromList, difference, empty, intersection, size )


getData :: IO [String]
getData = do
  txt <- readFile "Data/Day6.in"
  return $ lines txt


type Coord = (Int, Int)


parseCoord :: String -> Coord
parseCoord s = (read $ head ws, read $ ws!!1)
  where
    ws = splitOn "," s
    

type Rect = (Coord, Coord)


toList :: Rect -> [Coord]
toList ((x1, y1), (x2, y2)) = [(x,y) | x<-[x1..x2], y<-[y1..y2]]


toSet :: Rect -> Set Coord
toSet = fromList . toList


type Grid1 = Set Coord
type Grid2 = Map Coord Int


data Action = TOGGLE | ON | OFF


parseLine :: String -> (Action, Rect)
parseLine s 
  | head ws == "toggle" = (TOGGLE, (parseCoord $ ws!!1, parseCoord $ ws!!3))
  | ws!!1 == "on" = (ON, (parseCoord $ ws!!2, parseCoord $ ws!!4))
  | ws!!1 == "off" = (OFF, (parseCoord $ ws!!2, parseCoord $ ws!!4))
  | otherwise = error "Error parsing line"
  where
    ws = words s


run1 :: Grid1 -> (Action, Rect) -> Grid1
run1 g (ON, r) = g `union` toSet r
run1 g (OFF, r) = g `difference` toSet r
run1 g (TOGGLE, r) = (g `union` toSet r) `difference` (g `intersection` toSet r)


run2 :: Grid2 -> (Action, Rect) -> Grid2
run2 g (ON, r) = foldr ( M.adjust (+1)) g $ toList r
run2 g (OFF, r) = foldr ( M.adjust (\x -> max 0 (x-1))) g $ toList r
run2 g (TOGGLE, r) = foldr ( M.adjust (+2)) g $ toList r


startGrid1 :: Grid1
startGrid1 = empty --M.TMap False $ M.fromList $ zip [(x, y)| x<-[0..999], y<-[0..999]] $ repeat False

startGrid2 :: Map Coord Int
startGrid2 = M.fromList [((x,y), 0) | x <-[0..999], y<-[0..999]]

day6 :: IO ()
day6 = do
  ss <- getData
  let ls = parseLine <$> ss
      endGrid1 :: Grid1
      endGrid1 = foldl run1 startGrid1 ls
      endGrid2 = foldl run2 startGrid2 ls
  putStrLn $ "Day6: part1: " ++ show (size endGrid1)
  putStrLn $ "Day6: part1: " ++ show (sum endGrid2)

  return ()


--instance Semigroup Int where
  --x <> y = x+y

--instance Monoid Int where
  --mempty = 0

