{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Day05.HydrothermalVenture where
import Data.Array ( accumArray, elems )
import Data.List.Split ( dropBlanks, dropDelims, oneOf, split )
main :: IO ()
main = do
  segments <- map parseLine . lines <$> readFile "data/day05.txt"
  let points = concatMap (\(x,y) -> [x,y]) segments
      bnds = let xs = map fst points; ys = map snd points in ((minimum xs, minimum ys), (maximum xs, maximum ys))
      grid = accumArray (+) 0 bnds . map (,1) $ concatMap segmentCoords segments
  print (length . filter (>1) . elems $grid)
  where
    parseLine = toCoord . map (read @Int) . split (dropDelims . dropBlanks $ oneOf ",->")
    toCoord [x1, y1, x2, y2] = ((x1, y1), (x2, y2))

segmentCoords :: ((Int, Int), (Int, Int)) -> [(Int, Int)]
segmentCoords ((x1, y1), (x2, y2)) = zip xs ys
  where xs = if x1 == x2 then repeat x1 else range' x1 x2
        ys = if y1 == y2 then repeat y1 else range' y1 y2
        range' a b = [a,(a + if b < a then -1 else 1)..b]