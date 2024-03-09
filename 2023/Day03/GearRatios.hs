{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
import           Data.Char
import           Data.List (nub, sort, transpose)

main :: IO ()
main = do
    xss <- lines <$> readFile "2023/Day03/day03.txt"

    let part1 = adjacentNums (\c -> c /= '.' && (not . isDigit) c) xss
    print $ sum [x | (x, labels) <- part1, not . null $ labels]

    -- let part2 = adjacentNums (== '*') xss
    let tmp = group . sort $ [(l, [x]) | (x, labels) <- part1, l <- labels]
    print $ sum [product nums | (_, nums) <- tmp, length nums == 2]
  where
    group as = foldr (\(l1, xs) ((l2, ys) : res) -> if l1 == l2 then (l1, xs++ys):res else (l1, xs):(l2, ys):res) [last as] (init as)

adjacentNums :: (Char -> Bool) -> [[Char]] -> [(Integer, [Int])]
adjacentNums isSym xss =
    let labels = zipWith (zipWith (\x c -> ([c | isSym x]))) xss (iterate (drop 150) [1..])
        labels' = transpose $ expand (transpose (expand labels))
    in [x | row <- zipWith zip xss labels', x <- readNums row]
  where
    expand = map ((\xs -> zipWith3 (\a b c -> let cs = a++b++c in cs) xs (tail xs) ((tail . tail) xs ++ repeat [])) . ([]:))
    readNums :: [(Char, [Int])] -> [(Integer, [Int])]
    readNums [] = []
    readNums xs | null ds = readNums (tail xs)
            | otherwise = (num, labels) : readNums xs'
      where
        (ds, xs') = span (isDigit . fst) xs
        num = read . map fst $ ds
        labels = nub [l | (_, lbls) <- ds, l <- lbls]
