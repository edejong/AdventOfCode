import           Data.Char ()

main :: IO ()
main = do
  xs <- map (read @Int) . lines <$> readFile "2021/Day01/day01.txt"
  print $ map (\n -> length (filter (uncurry (<)) (zip xs (drop n xs)))) [1, 3]
