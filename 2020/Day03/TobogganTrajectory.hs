main :: IO ()
main = do
    rows <- lines <$> readFile "2020/data/day03.txt"
    print $ foldr ((*) . uncurry (slide rows)) 1 [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]

slide :: [[Char]] -> Int -> Int -> Int
slide rows right down =
    let rows' = [row | (i,row) <- zip [0..] rows, i `mod` down == 0]
        width = length . head $ rows
        shifts = map (`mod` width) [0,right..] in
    length . filter (== '#') . zipWith (!!) rows' $ shifts
