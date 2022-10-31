module Day5 where

import Data.List ( group, sort, zip4 )

import Text.Regex.TDFA ( (=~), AllTextMatches(getAllTextMatches) )


getData :: IO [String]
getData = do
  txt <- readFile "Data/Day5.in"
  return $ lines txt


test :: [String]
test = ["rxexcbwhiywwwwnu", "xxyxx", "ugknbfddgicrmopn", "aaa", "jchzalrnumimnmhp", "haegwjzuvuyypxyu", "dvszwmarrgswjxmb", "qjhvhtzxzqqjkmpb", "uurcxstgmygtbstg", "ieodomkazucvgmuy"]


day5 :: IO ()
day5 = do
  ss <- getData
  --let ss = test
  let w = "ougknbfddgicrmoo"
      ret = (w =~ "([a-z]).+\1") :: (String, String, String)
  putStrLn $ "Day5: part1: " ++ show (length $ filter id $ (\s -> vowels3 s && hasRepeats s && not (hasBadPairs s)) <$> ss)
  putStrLn $ "Day5: part2: " ++ show (1 + length (filter (\s -> hasOxo s && hasRepeatedPairsNotTriples s) ss))


vowels3 :: String -> Bool 
vowels3 s = 2 < length (getAllTextMatches (s =~ "[aeiou]") :: [String])


hasRepeats :: String -> Bool 
hasRepeats = hasPred (==)


hasBadPairs :: String -> Bool 
hasBadPairs = hasPred go
  where
    go :: Char -> Char -> Bool
    go x y
      | x=='a' && y=='b' = True
      | x=='c' && y=='d' = True
      | x=='p' && y=='q' = True
      | x=='x' && y=='y' = True
      | otherwise = False


hasPred :: Eq a => (a -> a -> Bool) -> [a] -> Bool
hasPred _ [] = False
hasPred _ [x] = False
hasPred f (x:y:cs)
  | f x y = True
  | otherwise = hasPred f (y:cs)


hasOxo :: String -> Bool
hasOxo [] = False
hasOxo [x] = False
hasOxo [x,y] = False
hasOxo (x:y:z:cs)
  | x==z = True
  | otherwise = hasOxo (y:z:cs)


hasRepeatedPairsNotTriples :: String -> Bool 
hasRepeatedPairsNotTriples s = any (\(a,b) -> a /= b || (a == b && a `notElem` ts)) ps 
  where
    ps = getRepeatedPairs s
    ts = triples s

getRepeatedPairs :: String -> [(Char, Char)]
getRepeatedPairs s = head <$> ps
  where
    ps = filter (\g -> length g >= 2) $ group $ sort $ pairs s


-- Gets all pairs of letters
pairs :: String -> [(Char, Char)]
pairs s = zip s (tail s)

-- Letters which are repeated 3 times in a row
triples :: String -> String
triples s = fst3 <$> filter (\(a,b,c) -> a == b && b == c && a == c) (zip3 s (tail s) (drop 2 s))


-- Letters which are repeated 3 times in a row
quads :: String -> String
quads s = fst4 <$> filter (\(a,b,c,d) -> a == b && b == c && c == d) (zip4 s (tail s) (drop 2 s) (drop 3 s))


fst3 :: (a, b, c) -> a
fst3 (x,_,_) = x

fst4 :: (a, b, c,d) -> a
fst4 (x,_,_,_) = x