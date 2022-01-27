{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Day13.TransparentOrigami where
import Data.Array
    ( Ix(range), (//), bounds, elems, listArray, Array )
import Data.Bifunctor ( Bifunctor(second, first) )
import Data.List ( nub, transpose )
import Data.List.Split ( chunksOf, splitOn )

type Point = (Int, Int)
type Grid = Array Point Char

main :: IO ()
main = do
  [points, folds] <- map lines . splitOn "\n\n" <$> readFile "2021/data/day13.txt"
  let ps = map (tuple . map (read @Int) . splitOn ",") points
      fs = map (second (read @Int) . tuple . splitOn "=" . drop 11) folds
  print $ length (doFolds ps [head fs])         -- part 1
  putStrLn $ printGrid . mkGrid $ doFolds ps fs -- part 2
  where
    tuple [a, b] = (a, b)

doFolds :: [Point] -> [(String, Int)] -> [Point]
doFolds = foldl (flip doFold)
  where
    doFold ("x", k) = nub . filter ((< k) . fst) . map (first (foldFunc k))
    doFold ("y", k) = nub . filter ((< k) . snd) . map (second (foldFunc k))
    foldFunc k x = if x < k then x else 2*k-x

mkGrid :: [Point] -> Grid
mkGrid ps = listArray bnds ['.' | i <- range bnds] // [(i, '#') | i <- ps]
  where
    bnds = (\(xs, ys) -> ((minimum xs, minimum ys), (maximum xs, maximum ys))) (map fst ps, map snd ps)

printGrid :: Grid -> String
printGrid grid = unlines . transpose . chunksOf width $ elems grid
 where
   bnds = bounds grid
   width = 1 + (snd . snd $ bnds) - (snd . fst $ bnds)