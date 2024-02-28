import           Data.List.Split (splitOn)

main :: IO ()
main = do
  xs <- map (read @Int) . splitOn "," <$> readFile "2021/Day07/day07.txt"
  print (minFuel id xs, minFuel (\n -> n * (n + 1) `div` 2) xs) -- (part1, part2)
  where
    minFuel f xs =
      minimum [sum . map (f . abs . subtract k) $ xs | k <- [minimum xs .. maximum xs]]
