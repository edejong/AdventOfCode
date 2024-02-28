import           Data.List.Split (chunksOf)

main :: IO ()
main = do
    xs <- tick 1 . lines <$> readFile "2022/data/Day10.txt"
    print $ sum . map head . chunksOf 40 . drop 19 . zipWith (*) [1..] $ xs
    putStr $ unlines . map printLine . chunksOf 40 $ xs
  where
    printLine = zipWith (\ a b -> (if abs (a - b) <= 1 then '#' else '.')) [0..]

tick :: Int -> [String] -> [Int]
tick x [] = [x]
tick x ("noop":ops) = x : tick x ops
tick x (op:ops) = let a = (read @Int . drop 5) op in [x, x] ++ tick (x+a) ops
