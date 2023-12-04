{-# LANGUAGE OverloadedStrings #-}
module Day04.Scratchcards where
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Text.Read as T
import Data.List (intersect, foldl')

main :: IO ()
main = do
    xs <- map ((map (map (either error fst . T.decimal) . T.words) . T.splitOn "|") . (!!1) . T.splitOn ":") . T.lines <$> T.readFile "2023/data/day04.txt"
    print $ sum $ map (score . scratch) xs
    print $ sum $ scorePart2 (map scratch xs) (repeat 1)
  where
    scratch [as, bs] = length (as `intersect` bs)
    score n = if n == 0 then 0 else 2 ^ (n-1)
    scorePart2 (n:ns) (c:cs) = c : scorePart2 ns (zipWith (+) (replicate n c ++ repeat 0) cs)
    scorePart2 _ _ = []
