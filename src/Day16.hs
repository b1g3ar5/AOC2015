{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Day16 where

import Utils
import qualified Data.Map as M

type Attribute = String

type Aunt = Map Attribute Int


parseAunt :: String -> Aunt
parseAunt s = M.fromList [(init $ bs!!2, read $ init $ bs!!3), (init $ bs!!4, read $ init $ bs!!5), (init $ bs!!6, read $ bs!!7)]
  where
    bs = words s


facts :: [(Attribute, Int)]
facts = [("children",  3)
  , ("cats", 7)
  , ("samoyeds", 2)
  , ("pomeranians", 3)
  , ("akitas", 0)
  , ("vizslas", 0)
  , ("goldfish", 5)
  , ("trees", 3)
  , ("cars", 20)
  , ("perfumes", 1)
  ]


getQualifyingAunts1 :: [Aunt] -> [Aunt]
getQualifyingAunts1 as = do
  a <- as
  sequence_ $ (\(p, v) -> guard $ fromMaybe v (a M.!? p) == v ) <$> facts
  return a

getQualifyingAunts2 :: [Aunt] -> [Aunt]
getQualifyingAunts2 as = do
  a <- as
  sequence_ $ (\(p, v) -> guard $ guardFunction a v p ) <$> facts
  return a

guardFunction :: Aunt -> Int -> String -> Bool
guardFunction a v p
  | p == "cat" = fromMaybe (v+1) (a M.!? p) > v
  | p == "trees" = fromMaybe (v+1) (a M.!? p) > v
  | p == "pomeranians" = fromMaybe (v-1) (a M.!? p) < v
  | p == "goldfish" = fromMaybe (v-1) (a M.!? p) < v
  | otherwise = fromMaybe v (a M.!? p) == v
  

day16 :: IO ()
day16 = do
  ss <- getLines 16
  let as = parseAunt <$> ss
      a1 = head $ getQualifyingAunts1 as
      a2 = head $ getQualifyingAunts2 as
  putStrLn $ "Day16: part1: " ++ show (filter (\(ix, b) -> b == a1) $ zip [1..] as)
  putStrLn $ "Day16: part2: " ++ show (filter (\(ix, b) -> b == a2) $ zip [1..] as)

  return ()
