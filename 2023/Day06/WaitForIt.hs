{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

import           Data.Char (isDigit)

main :: IO ()
main = do
    xs <- map (drop 10) . lines <$> readFile "2023/Day06/day06-test.txt"
    print (solve xs, solve (map (filter isDigit) xs))

solve :: [String] -> Integer
solve = product . map (uncurry f) . (\[as,bs] -> zip as bs) . map (map read . words)
  where f t r = let (x1, x2) = roots (-1) t (-r) in ceiling (x2 - 1) - floor (x1 + 1) + 1

roots :: Double -> Double -> Double -> (Double, Double)
roots a b c = let x = sqrt (b^(2::Int) - 4*a*c) in ((-b + x) / (2*a), (-b - x) / (2*a))
