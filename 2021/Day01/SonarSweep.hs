module Day01.SonarSweep where
import Data.Char ()
import Data.List (tails)
import Data.Maybe (catMaybes, fromJust)

main :: IO ()
main = do
  xs <- map (read :: String -> Int) . lines <$> readFile "data/day01.txt"
  print $ map (\n -> length (filter (uncurry (<)) (zip xs (drop n xs)))) [1, 3]
