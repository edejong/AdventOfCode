module Day02.RockPaperScissors where
import Data.Char (ord)

main :: IO ()
main = do
    xs <- map (\s -> (ord (head s) - ord 'A', ord (s !! 2) - ord 'X')) . lines <$> readFile "2022/data/day02.txt"
    print (sum . map f1 $ xs, sum . map f2 $ xs)
  where
    f1 (x, y) = 3 * ((y - x + 1) `mod` 3) + (y + 1)
    f2 (x, y) = (y * 3) + ((x + y + 2) `mod` 3) + 1