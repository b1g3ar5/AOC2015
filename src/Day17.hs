{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor #-}

module Day17 where

import Utils
import Control.Arrow ( (>>>), (<<<) )


day17 :: IO ()
day17 = do
  ss <- getLines 17
  let jugs :: [Int]
      jugs = reverse $ sort $ read <$> ss
      minLength = minimum $ (length <$>) $ filter (\s -> sum s == 150) $ subsequences jugs
      minSequences = filter (\s -> sum s == 150 && length s == minLength) $ subsequences jugs
  putStrLn $ "Day17: part1: " ++ show (length $ filter (\s -> sum s == 150) $ subsequences jugs)
  putStrLn $ "Day17: part1: " ++ show (length minSequences)
  
  return ()
