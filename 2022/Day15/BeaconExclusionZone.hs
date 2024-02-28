{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
import           Data.List       (nub, sort)
import           Data.List.Split (splitOn)
import           Data.Maybe      (mapMaybe)

-- TODO: Part 2 is brute force. It finishes in a few seconds, but it could
-- probably be improved a lot.

main :: IO ()
main = do
    xs <- map parseLine . lines <$> readFile "2022/Day15/day15.txt"
    -- let row = 10
    let row = 2000000
        (lo, hi) = (0, 2*row)
        beaconsOnRow = nub . map fst . filter ((== row) . snd) . map snd $ xs
        rs = mergeSortedRanges . sort . mapMaybe (uncurry (overlap row)) $ xs
        part1 = sum . map rangeLength . removeFromRanges beaconsOnRow $ rs

    let rows = [lo..hi]
        rss = map (\row' -> mergeSortedRanges . sort . mapMaybe (uncurry (overlap row')) $ xs) rows
        [(y, [(_,x),_])] = filter (\(_,r) -> r /= [(lo,hi)]) $ zip [0..] . map (map (clampRange lo hi)) $ rss
        part2 = (x+1) * 4000000 + y

    print (part1, part2)

type Point = (Int, Int)

parseLine :: String -> (Point, Point)
parseLine line = let [s, b] = splitOn ": " line in (parseCoord . drop 10 $ s, parseCoord . drop 21 $ b)

parseCoord :: String -> Point
parseCoord str = let [x, y] = splitOn ", " str in (read @Int . drop 2 $ x, read @Int . drop 2 $ y)

overlap :: Int -> Point -> Point -> Maybe (Int, Int)
overlap row sensor@(x, y) beacon = overlap' $ manhattanDist sensor beacon
  where overlap' dist
          | abs (row - y) > dist = Nothing
          | otherwise = let d = abs (row - y); d' = abs (d - dist) in Just (x-d', x+d')

manhattanDist :: Point -> Point -> Int
manhattanDist (x1, y1) (x2, y2) = sum . map abs $ [x1 - x2, y1 - y2]

mergeSortedRanges :: [(Int, Int)] -> [(Int, Int)]
mergeSortedRanges (a@(a1,a2):b@(b1,b2):rs)
  | a2 < (b1-1) = a : mergeSortedRanges (b:rs)
  | b2 <= a2 = mergeSortedRanges (a:rs)
  | a2 >= (b1-1) = mergeSortedRanges ((a1,b2):rs)
mergeSortedRanges rs = rs

removeFromRanges :: [Int] -> [(Int, Int)] -> [(Int, Int)]
removeFromRanges (x:xs) (a@(a1,a2):rs)
  | x < a1 = removeFromRanges xs (a:rs)
  | x >= a1 && x <= a2 = removeFromRanges xs (removeFromRange x a <> rs)
  | otherwise = a : removeFromRanges (x:xs) rs
removeFromRanges _ rs = rs

removeFromRange :: Int -> (Int, Int) -> [(Int, Int)]
removeFromRange x (a, b)
  | x < a || x > b = [(a, b)]
  | otherwise = filter isValid [(a, x-1), (x+1, b)]
  where
    isValid (a', b') = b' >= a'

rangeLength :: (Int, Int) -> Int
rangeLength (a, b) = b - a + 1

clampRange :: Int -> Int -> (Int, Int) -> (Int, Int)
clampRange lo hi (a, b) = (max lo a, min hi b)
