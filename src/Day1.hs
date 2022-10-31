module Day1(
  day1
  ) where


day1 :: IO ()
day1 = do
  txt <- getData
  --let out1 = foldr (\xa xb -> xb <> parse xa) mempty txt
  let out1 = foldMap parse txt
  putStrLn $ "Day1: Part1: " ++ show (lefts out1 - rights out1)
  let out2 = foldl (\xb xa -> xb <> iparse xa) mempty $ zip txt [1..]
  putStrLn $ "Day1: Part2: " ++ show (ix out2)
  return ()


getData :: IO String
getData = readFile "Data/Day1.in"


-- The simple bracket
data Bracket = Bracket {
  rights :: !Int
  , lefts :: !Int
  } deriving (Show, Eq)

parse :: Char -> Bracket
parse '(' = Bracket 0 1
parse ')' = Bracket 1 0
parse _ = Bracket 0 0

instance Semigroup Bracket where
  (Bracket a b) <> (Bracket c d) =
    if b >= c then
      Bracket a (d+b-c)
      else
        Bracket (a+c-b) d

instance Monoid Bracket where
  mempty = Bracket 0 0

-- The indexed bracket
data IBracket = IBracket {
  ix :: Int
  , irights :: !Int
  , ilefts :: !Int
  } deriving (Show, Eq)

iparse :: (Char, Int) -> IBracket
iparse ('(', x) = IBracket x 0 1
iparse (')', x) = IBracket x 1 0
iparse (_, x) = IBracket x 0 0


instance Semigroup IBracket where
  (IBracket x 1 b) <> _ = IBracket x 1 b
  (IBracket x 0 b) <> (IBracket y c d) =
    if b >= c then
      IBracket 0 0 (d+b-c)
      else
        IBracket y 1 d

instance Monoid IBracket where
  mempty = IBracket 0 0 0
