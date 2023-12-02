{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
module Day19.NotEnoughMinerals where

main :: IO ()
main = do
    xs <- readFile "2022/data/day19-test.txt"
    print xs

-- data MaterialType = Ore | Clay | Obsidian | Geode
-- data Robot = Robot MaterialType
-- type Stash = Stash Int MaterialType Int MaterialType Int MaterialType Int MaterialType
newtype Ore = Ore Int
newtype Clay = Clay Int
newtype Obsidian = Obsidian Int
newtype Geode = Geode Int
type Bla = (Ore, Clay, Obsidian, Geode)