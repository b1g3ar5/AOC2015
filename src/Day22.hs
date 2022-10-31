{-# LANGUAGE FlexibleContexts, NamedFieldPuns, RecordWildCards, ViewPatterns #-}

module Day22 where

import Control.Monad.State (MonadState, evalState, gets, modify)
import Data.Heap (MinPrioHeap)
import qualified Data.Heap as H (insert, singleton, view)
import Data.Map (Map)
import qualified Data.Map as M (empty, filter, insertWith, keys, lookup, member)
import Data.Set (Set)
import qualified Data.Set as S (empty, insert, member)

-- Based on https://github.com/ephemient/aoc2015/blob/master/src/Day22.hs

data Me = Me {mePoints:: Int, mana :: Mana} deriving (Eq, Show, Ord)
data Boss = Boss { bossPoints :: Int, damage :: Int} deriving (Eq, Show, Ord)
data Game = MyTurn {me :: Me, boss :: Boss, spells :: Map Spell Int}
          | BossTurn {me :: Me, boss :: Boss, spells :: Map Spell Int} deriving (Eq, Show, Ord)
type Mana = Int

start :: Game
start = MyTurn (Me 50 500) (Boss 71 10) M.empty


data Spell = MagicMissile | Drain | Shield | Poison | Recharge deriving (Show, Eq, Bounded, Ord)


cast :: Spell -> Game -> Game
cast MagicMissile gs = gs {boss = (boss gs) { bossPoints = bossPoints (boss gs) - 4}}
cast Drain gs = gs { boss = (boss gs) {bossPoints = bossPoints (boss gs) - 2}
                   , me = (me gs) { mePoints = mePoints (me gs) + 2}
                   }
cast Shield gs = gs {spells = M.insertWith (+) Shield 6 $ spells gs}
cast Poison gs = gs {spells = M.insertWith (+) Poison 6 $ spells gs}
cast Recharge gs = gs {spells = M.insertWith (+) Recharge 5 $ spells gs}


affectMe :: Spell -> Me -> Me
affectMe Recharge me = me {mana = mana me + 101}
affectMe _ me = me


affectBoss :: Spell -> Boss -> Boss
affectBoss Poison b = b { bossPoints = bossPoints b - 3}
affectBoss _ b = b


shop :: [(Spell, Mana)]
shop = zip [Shield, Poison, Recharge, MagicMissile, Drain] [113, 173, 229, 53, 73]

minCost :: Mana
minCost = 53


safeHead :: Monoid a => [a] -> a
safeHead [] = mempty
safeHead (x:xs) = x
    

play :: (Monad m, MonadState (Set Game) m) => Int -> MinPrioHeap Int Game -> m (Maybe Int)
play decay (H.view -> Just ((total, game), heap)) = do
  seen <- gets $ S.member game
  if seen then 
    play decay heap 
  else
    case game of
      _ | bossPoints (boss game) <= 0 -> return $ Just total
      _ | mePoints (me game) <= 0 -> play decay heap
      MyTurn {} -> total `seq` play decay $ foldr H.insert heap
          [ (total + cost, cast spell game')
          | (spell, cost) <- shop
          , not $ M.member spell spells'
          , cost <= mana me''
          , let game' = BossTurn
                  { me = me'' {mana = mana me'' - cost}
                  , boss = boss'
                  , spells = spells'
                  }
          ]
      BossTurn {} -> play decay $ H.insert
          ( total
          , MyTurn{ me = me'' {mePoints = mePoints me'' - max 1 (damage boss' - armour)}
                  , boss = boss'
                  , spells = spells'
                  }
          ) heap
  where
    me'
      | MyTurn {me} <- game = me {mePoints = mePoints me - decay}
      | BossTurn {me} <- game = me
    me'' = foldr affectMe me' . M.keys $ spells game
    boss' = foldr affectBoss (boss game) . M.keys $ spells game
    spells' = M.filter (> 0) $ subtract 1 <$> spells game
    armour = if M.member Shield $ spells game then 7 else 0
play _ _ = return Nothing

-- 1824
-- 1937
day22 :: IO ()
day22 = do
  putStrLn $ "Day22: part1: " ++ show (flip evalState S.empty . play 0 $ H.singleton (0, start))
  putStrLn $ "Day22: part2: " ++ show (flip evalState S.empty . play 1 $ H.singleton (0, start))

  return ()


