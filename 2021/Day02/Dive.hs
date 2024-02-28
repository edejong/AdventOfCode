{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
import           Data.List (foldl')

main :: IO ()
main = do
  result <- foldl' f (0, 0, 0) . map (parse . words) . lines <$> readFile "2021/Day02/day02.txt"
  print ((\(h, d, a) -> (h * a, h * d)) (result :: (Int, Int, Int))) -- (part1, part2)
  where
    f (h, d, a) (x, y) = (h + x, d + a * x, a + y)
    parse [dir : _, n] =
      let n' = read n
       in case dir of 'f' -> (n', 0); 'd' -> (0, n'); 'u' -> (0, - n')
