main :: IO ()
main = do
    xs <- map (map (read @Integer) . words) . lines <$> readFile "2023/Day09/day09.txt"
    print (sum $ map next xs, sum $ map prev xs)
  where
    next xs | all (==0) xs = 0
            | otherwise = let xs' = zipWith subtract xs (tail xs) in last xs + next xs'
    prev xs | all (==0) xs = 0
            | otherwise = let xs' = zipWith subtract xs (tail xs) in head xs - prev xs'
