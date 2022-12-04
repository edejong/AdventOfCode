{-# LANGUAGE TypeApplications #-}
module Day04.CampCleanup where
import Data.List.Split (splitOn)

main :: IO ()
main = do
    xs <- map (map (map (read @Int) . splitOn "-"). splitOn ",") . lines <$> readFile "2022/data/day04.txt"
    let f1 [a, b] = a `contains` b || b `contains` a
    let f2 [a, b] = a `overlaps` b
    print (length . filter f1 $ xs, length . filter f2 $ xs)
  where
    contains [a, b] [c, d] = (a >= c && b <= d) || (a <= c && b >= d)
    overlaps [a, b] [c, d] = a <= d && c <= b
