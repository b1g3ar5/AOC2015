{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Day19 where

import Utils
import Data.Ord

type Rule = (String, String)

parseRule :: String -> Rule
parseRule s = (head bs, bs!!2)
  where
    bs = words s


target :: String
target = "CRnSiRnCaPTiMgYCaPTiRnFArSiThFArCaSiThSiThPBCaCaSiRnSiRnTiTiMgArPBCaPMgYPTiRnFArFArCaSiRnBPMgArPRnCaPTiRnFArCaSiThCaCaFArPBCaCaPTiTiRnFArCaSiRnSiAlYSiThRnFArArCaSiRnBFArCaCaSiRnSiThCaCaCaFYCaPTiBCaSiThCaSiThPMgArSiRnCaPBFYCaCaFArCaCaCaCaSiThCaSiRnPRnFArPBSiThPRnFArSiRnMgArCaFYFArCaSiRnSiAlArTiTiTiTiTiTiTiRnPMgArPTiTiTiBSiRnSiAlArTiTiRnPMgArCaFYBPBPTiRnSiRnMgArSiThCaFArCaSiThFArPRnFArCaSiRnTiBSiThSiRnSiAlYCaFArPRnFArSiThCaFArCaCaSiThCaCaCaSiRnPRnCaFArFYPMgArCaPBCaPBSiRnFYPBCaFArCaSiAl"


-- Applies a single rule and returns all the new strings
apply :: String -> Rule ->  [String]
apply [] r = []
apply str ([], s) = error "A rule must have a string to replace"
apply str (r, s) = go [] [] str
  where
    rl = length r
    go :: [String] -> String -> String -> [String]
    go acc front  [] = acc
    go acc front xs
      | length xs < length r = acc
      | f == r = go ((front ++ s ++ b):acc) (front++f) b
      | otherwise = go acc (front++[head xs]) (tail xs)
      where
        (f, b) = splitAt rl xs


reduce :: Int ->  String -> [Rule] -> Maybe Int
reduce n start rs 
  | start == "e" = Just n -- If the string is "e" then we've finished
  | 'e' `elem` start = Nothing  -- If there is an e in the string that's no good
  | null ret = Nothing -- If none of the rules can be applied then fail
  | otherwise = Just $ head ret -- Take the first successful reductions is this the shortest?
  where
    -- Strings from the application of one rule
    ss = nub $ concat $ apply start <$> rs
    -- Apply reduce to all these reduced strings
    ret = catMaybes $ (\s -> reduce (n+1) s rs) <$> ss


day19 :: IO ()
day19 = do
  ss <- getLines 19
  --let ss = test
  let rs1 = parseRule <$> take 43 ss
      rs2 = sortOn (Down . length . fst) $ swap <$> rs1
  putStrLn $ "Day19: part1: " ++ show (length $ nub $ concat $ apply target <$> rs1)
  putStrLn $ "Day19: part1: " ++ show (reduce 0 target rs2)

  return ()

