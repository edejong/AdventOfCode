module Day09.SmokeBasin where

import Data.Array (Array, array, bounds, indices, (!))
import Data.Char (digitToInt)
import Data.List (sort, sortOn, (\\))
import Data.Ord (Down (Down), comparing)

type Point = (Int, Int)

main :: IO ()
main = do
  grid <- gridFrom . map (map digitToInt) . lines <$> readFile "2021/data/day09.txt"
  let lowPoints = [i | i <- indices grid, (grid ! i) <= (minimum . map (grid !) . neighbours (bounds grid) $ i)]
  print $ (sum . map (grid !) $ lowPoints) + length lowPoints
  print $ product . take 3 . sortOn Down . map (length . dfs grid) $ lowPoints

neighbours :: (Point, Point) -> Point -> [Point]
neighbours (lo, hi) (x, y) = filter inBounds [(x -1, y), (x + 1, y), (x, y -1), (x, y + 1)]
  where
    inBounds (x', y') = x' >= fst lo && x' <= fst hi && y' >= snd lo && y' <= snd hi

gridFrom :: [[e]] -> Array Point e
gridFrom xs = array ((0, 0), (length xs - 1, length (head xs) - 1)) coords
  where
    coords = [((y, x), v) | (y, row) <- zip [0 ..] xs, (x, v) <- zip [0 ..] row]

dfs :: Array Point Int -> Point -> [Point]
dfs grid i = dfs' [i] []
  where
    dfs' [] result = result
    dfs' (i : is) result
      | i `elem` result = dfs' is result
      | otherwise = let js = (filter (\j -> (grid ! j) < 9) . neighbours bnds $ i) in 
          dfs' (js ++ is) (i : result)
    bnds = bounds grid