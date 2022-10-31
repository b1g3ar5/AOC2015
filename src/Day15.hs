{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Day15 where

import Utils (guard, getLines, transpose)


type Props = [Int]


parseLine :: String -> Props
parseLine s = [read $ init $ bs!!2, read $ init $ bs!!4, read $ init $ bs!!6, read $ init $ bs!!8, read $ bs!!10]
  where
    bs = words s


score :: [(Props, Int)] -> Int
score ips = if any (<0) props then 0 else product props
  where
    props = (sum  <$>) <$> transpose $ (\(ps, spoons) -> (spoons*) <$> ps) <$> ips


day15 :: IO ()
day15 = do
  ss <- getLines 15
  let ps = parseLine <$> ss
      cs = last <$> ps
      recipes :: [[Int]]
      recipes = [[f, c, b, s] | f <- [0..100], c <- [0..(100-f)], b <- [0..(100-f-c)], s <- [100-f-c-b]]
      recipes500 = do
        f <- [0..100]
        c <- [0..(100-f)]
        b <- [0..(100-f-c)]
        s <- [100-f-c-b]
        guard $ sum (zipWith (*) [f,c,b,s] cs) == 500
        return [f,c,b,s]
  putStrLn $ "Day15: part1: " ++ show (maximum $ score . zip (take 4 <$> ps) <$> recipes)
  putStrLn $ "Day15: part1: " ++ show (maximum $ score . zip (take 4 <$> ps) <$> recipes500)

  return ()

