{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

import           Data.Function (on)
import           Data.List     (sort, tails, transpose)

main :: IO ()
main = do
  xss <- lines <$> readFile "2023/Day11/day11.txt"
  let emptyIndices = map fst . filter (all (== '.') . snd) . zip [0 ..]
  let emptyIndices' = (emptyIndices xss, emptyIndices . transpose $ xss)
  let galaxyPairs = pairs [(row, col) | (row, xs) <- zip [0 ..] xss, (col, x) <- zip [0 ..] xs, x == '#']
  print $ sum . map (uncurry (totalDist emptyIndices' 2)) $ galaxyPairs
  print $ sum . map (uncurry (totalDist emptyIndices' 1000000)) $ galaxyPairs

pairs :: [b] -> [(b, b)]
pairs xs = [(x, y) | (x : ys) <- tails xs, y <- ys]

totalDist :: ([Integer], [Integer]) -> Integer -> (Integer, Integer) -> (Integer, Integer) -> Integer
totalDist (emptyRows, emptyCols) d g1 g2 =
  manhattanDist g1 g2 + (d - 1) * ((extraSteps emptyRows `on` fst) g1 g2 + (extraSteps emptyCols `on` snd) g1 g2)
  where
    extraSteps xs a b = let [a', b'] = sort [a, b] in fromIntegral . length . filter (\x -> x > a' && x < b') $ xs
    manhattanDist (x1, y1) (x2, y2) = sum . map abs $ [x1 - x2, y1 - y2]
