{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Day21 where

import Utils (guard, getLines, maximumBy, minimumBy, comparing)
import Conway3 (basicRule3)
import GHC.Exts (sortWith)


data Player = Player {hitPoints ::Int, damage ::Int, armour ::Int} deriving (Eq, Show)
data Item = Item {name :: String, cost :: Int, dam :: Int, arm :: Int} deriving (Eq, Show) 
type GameState = (Player, Player)


instance Semigroup Item where
  (Item i a b c) <> (Item j d e f) = Item (i++j) (a+d) (b+e) (c+f)


instance Monoid Item where
  mempty = Item "" 0 0 0


buy :: Player -> Item -> Player
buy (Player h _ _) (Item _ c id ia) = Player h id ia


getState :: [Item] -> GameState
getState items = (buy me (mconcat items), boss)


play :: GameState -> GameState
play (Player h1 d1 a1, Player h2 d2 a2) = 
  (Player (h1-damageInflicted2) d1 a1, Player (h2-damageInflicted1) d2 a2)
  where
    damageInflicted1 = max 1 (d1 - a2)
    damageInflicted2 = max 1 (d2 - a1)


loop :: GameState -> (Bool, GameState)
loop s@(Player h1 _ _, Player h2 _ _)
  | h2<=0 = (True, s)
  | h1<=0 = (False, s)
  | otherwise = loop (play s)


boss, me :: Player
boss = Player {hitPoints = 104, damage = 8, armour =  1}
me = Player {hitPoints = 100, damage = 0, armour =  0}


-- I added nothing items to the armour and the rings so
-- I didn't have to make them optional
getItems :: [[Item]]
getItems = do
  w <- weapons
  a <- protection
  r1 <- rings
  r2 <- rings
  guard $ r1 /= r2
  return [w, a, r1, r2]


day21 :: IO ()
day21 = do
  ss <- getLines 21
  let choices = getItems
      finishes = loop . getState <$> choices
      meWin = filter (fst . snd) $ zip choices finishes
      bossWin = filter (not . fst . snd) $ zip choices finishes

  putStrLn $ "Day21: part1: " ++ show (cost . mconcat . fst $ minimumBy (comparing (cost . mconcat . fst)) meWin)
  putStrLn $ "Day21: part1: " ++ show (cost . mconcat . fst $ maximumBy (comparing (cost . mconcat . fst)) bossWin)

  return ()


weapons, protection, rings :: [Item]
weapons = [Item "dagger" 8 4 0
  , Item "shortsword" 10 5 0
  , Item "warhammer" 25 6 0
  , Item "longsword" 40 7 0
  , Item "greataxe" 74 8 0
  ]
protection = [ Item "leather" 13 0 1
  , Item "chainmail" 31 0 2
  , Item "splintmail" 53 0 3
  , Item "bandedmail" 75 0 4
  , Item "platemail" 102 0 5
  , mempty
  ]
rings = [Item "damage1" 25 1 0
  , Item "damage2" 50 2 0
  , Item "damage3" 100 3 0
  , Item "defense1" 20 0 1
  , Item "defense2" 40 0 2
  , Item "defense3" 80 0 3
  , Item "noring1" 0 0 0
  , Item "noring2" 0 0 0
  ]
