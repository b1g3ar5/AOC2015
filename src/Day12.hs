{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Day12 where

import Utils
import Data.Aeson
--import Data.ByteString.Lazy hiding (putStrLn)
import Data.ByteString.Lazy.Char8 ( pack )
import Data.Foldable
import Data.Scientific


getNumbers :: Value -> [Int]
getNumbers (Object o) = foldl (\acc v -> acc ++ getNumbers v) [] o
getNumbers (Array a) = foldl (\acc v -> acc ++ getNumbers v) [] a
getNumbers (String _) = []
getNumbers (Number n) = [fromIntegral $ coefficient n]
getNumbers (Bool _) = []
getNumbers Null = []

getNotRed :: Value -> [Int]
getNotRed (Array a) = foldl (\acc v -> acc ++ getNotRed v) [] a
getNotRed (String _) = []
getNotRed (Number n) = [fromIntegral $ coefficient n]
getNotRed (Bool _) = []
getNotRed Null = []
getNotRed (Object o) = snd $ foldl go (False, []) o
  where
    go (isFinished, acc) v 
      | isFinished = (isFinished, acc)
      | v == String "red" = (True, [])
      | otherwise = (isFinished, acc ++ getNotRed v)


day12 :: IO ()
day12 = do
  ss <- getLines 12
  let obj :: Value
      obj = fromMaybe (error "Can't decode the object") $ decode $ pack $ unlines ss
  putStrLn $ "Day12: part1: " ++ show (sum $ getNumbers obj)
  putStrLn $ "Day12: part1: " ++ show (sum $ getNotRed obj)

  return ()
