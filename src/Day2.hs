module Day2(
  day2
  ) where


day2 :: IO ()
day2 = do
  bs <- getData
  --let out1 = foldr (\xa xb -> xb <> parse xa) mempty txt
  let paperRequired = sum $ paper <$> bs
  putStrLn $ "Day2: Part1: " ++ show paperRequired
  let ribbonRequired = sum $ ribbon <$> bs
  putStrLn $ "Day2: Part2: " ++ show ribbonRequired
  return ()


getData :: IO [Box]
getData = do
  txt <- readFile "Data/Day2.in"
  let ls = lines txt
  return $ parse <$> ls


parse :: String -> Box
parse s = Box (is!!0) (is!!1) (is!!2)
  where
    is :: [Int]
    is = read <$> (words $ (\c -> if c == 'x' then ' ' else c) <$> s)

data Box = Box Int Int Int


paper :: Box -> Int
paper (Box l w h) = 2*l*w+2*l*h+2*w*h+extra
  where
    extra = minimum [l*w, l*h, w*h]


ribbon :: Box -> Int
ribbon (Box l w h) = minimum ps + l*h*w
  where
    ps = [2*l+2*w, 2*l+2*h, 2*h+2*w]
