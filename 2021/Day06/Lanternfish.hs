{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Day06.Lanternfish where
import Data.List.Split (splitOn)

main :: IO ()
main = do
    tmp <- map (read @Int) . splitOn "," <$> readFile "data/day06.txt"
    let days = map sum . iterate incDay . hist $ tmp
    print (days !! 80, days !! 256)
  where hist xs = [length . filter (==k) $ xs | k <- [0..8]]
        incDay [x0,x1,x2,x3,x4,x5,x6,x7,x8] = [x1,x2,x3,x4,x5,x6,x7+x0,x8,x0]