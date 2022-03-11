{-# LANGUAGE TypeApplications #-}
module Day02.IWasToldThereWouldBeNoMath where
import Data.List.Split (splitOn)
import Data.List (sort)

main :: IO ()
main = do
    xs <- map (map (read @Int) . splitOn "x") . lines <$> readFile "2015/data/day02.txt"
    print $ sum . map calcPaper $ xs
    print $ sum . map calcRibbon $ xs
  where calcPaper [d1, d2, d3] = let xs = [d1 * d2, d1 * d3, d2 * d3] in sum xs * 2 + minimum xs
        calcRibbon xs = ((*2) . sum . take 2 . sort) xs + product xs
