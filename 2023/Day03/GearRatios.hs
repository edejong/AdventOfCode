module Day03.GearRatios where

import qualified Data.Vector as V
import Data.Char
import Data.List (transpose)
-- import qualified Data.Vector.Unboxed as V

main :: IO ()
main = do
    xss <- lines <$> readFile "2023/data/day03-test.txt"
    let xss' = transpose $ f (transpose (f xss))
    putStrLn $ unlines xss
    putStrLn $ unlines xss'

    let foo = zipWith zip xss xss'
    print $ sum [x | row <- foo, x <- nums row]

f = map ((\xs -> zipWith3 (\a b c -> if any isSym [a,b,c] then '#' else '.') xs (tail xs) ((tail . tail) xs ++ repeat '.')) . ('.':))

isSym :: Char -> Bool
isSym c = c /= '.' && (not . isDigit) c

nums :: [(Char, Char)] -> [Int]
nums xs | null xs = []
        | null as = nums (tail xs)
        | all ((== '.') . snd) as = nums bs
        | otherwise = num : nums bs
  where
    (as, bs) = span (isDigit . fst) xs
    num = read . map fst $ as