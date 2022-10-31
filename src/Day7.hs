{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# HLINT ignore "Use head" #-}

{- HLINT ignore "Use head" -}

module Day7 where

import Utils
import Data.Bits
import Data.Int
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M


-- Instructions
data Ins = VAL Var | NOT Var | AND Var Var | OR  Var Var | LSHIFT Var Int | RSHIFT Var Int deriving (Show, Read)


eval :: Ins -> Maybe Int
eval (AND (Right x) (Right y)) = Just $ x .&. y
eval (OR (Right x) (Right y)) = Just $ x .|. y
eval (LSHIFT (Right x) n) = Just $ x `shift` n
eval (RSHIFT (Right x) n) = Just $ x `shift` (-n)
eval (NOT (Right x)) = Just $ (65536 + complement x) `rem` 65536
eval (VAL (Right i)) = Just i
eval _ = Nothing


type Reg = String


type Var = Either Reg Int


-- Looks up the reg in the map
parseVar :: Map Reg Int -> Reg -> Either Reg Int
parseVar mp s = 
  if head s `elem` "0123456789" then
    Right $ read s
  else
    m2e s (mp M.!? s)


m2e :: i -> Maybe a -> Either i a
m2e v Nothing = Left v 
m2e v (Just x) = Right x


parseInstruction :: Map Reg Int -> String -> Ins
parseInstruction mp s = 
  case bs!!1 of
    "AND" -> AND (parseVar mp $ head bs) (parseVar mp $ bs!!2)
    "OR" -> OR (parseVar mp $ head bs) (parseVar mp $ bs!!2)
    "LSHIFT" -> LSHIFT (parseVar mp $ head bs) (read $ bs!!2)
    "RSHIFT" -> RSHIFT (parseVar mp $ head bs) (read $ bs!!2)
    _ -> if head bs == "NOT" then
           NOT (parseVar mp $ bs!!1)
         else
           if head (head bs) `elem` "0123456789" then 
             VAL (Right $ read $ head bs)
           else
             VAL (parseVar mp $ head bs)
  where 
    bs = words s


go :: Map Reg Int -> [String] -> Map Reg Int
go mp [] = mp
go mp (l:ls) = case eval (parseInstruction mp l) of
                  Just x -> go (M.insert (last $ words l) x mp) ls
                  Nothing -> go mp $ ls ++ [l] --put it on the end to be evaluated later


day7 :: IO ()
day7 = do
  lines <- getLines 7
  let ls = lines
      mp1 = go M.empty ls
      newb = fromMaybe (error "This shouldn't happen") $ mp1 M.!? "a"
      newLs = ls ++ [show newb ++ " -> b"]
      mp2 = go M.empty newLs
  putStrLn $ "Day6: part1: " ++ show (mp1 M.!? "a")
  putStrLn $ "Day6: part2: " ++ show (mp2 M.!? "a")

  return ()
