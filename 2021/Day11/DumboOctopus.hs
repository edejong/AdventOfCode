{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
import           Data.Char
import           Data.List (find)
import qualified Data.Map  as Map
import           GHC.Arr

type Point = (Int, Int)
type Grid = Array Point Int

main :: IO ()
main = do
    xs <- map (map digitToInt) . lines <$> readFile "2021/Day11/day11.txt"
    let bnds = ((0,0), (length xs-1, length (head xs)-1))
        grid = listArray bnds (concat xs)
        steps = iterate step (grid, 0)

        (_, n) = (!! 100) . iterate step $ (grid, 0)
        Just (n', _) = find (\(_, (g, _)) -> all (== 0) (elems g)) $ zip [0..] steps
    print (n, n' :: Int)

step :: (Grid, Int) -> (Grid, Int)
step (grid, n) = (grid1', n + n')
  where
    (grid1, n') = inc grid (map (,1) . indices $ grid)
    grid1' = listArray (bounds grid) . map (\x -> if x <= 9 then x else 0) . elems $ grid1

flash :: Grid -> [Point] -> (Grid, Int)
flash grid [] = (grid, 0)
flash grid xs = (grid', n)
  where
    increments = map (,1) . concatMap (neighbours (bounds grid)) $ xs
    increments' = Map.toList . foldr (\(k, v) m -> Map.insertWith (+) k v m) Map.empty $ increments
    (grid', n) = inc grid increments'

inc :: Grid -> [(Point, Int)] -> (Grid, Int)
inc grid [] = (grid, 0)
inc grid xs = (grid'', n + length flashPoints)
  where
    grid' = accum (+) grid xs
    flashPoints = [c | (c, n') <- xs, let x = grid ! c, x < 10 && (x+n') >= 10]
    (grid'', n) = flash grid' flashPoints

neighbours :: (Point, Point) -> Point -> [Point]
neighbours (lo, hi) (x, y) = filter inBounds [(x+i, y+j) | i <- [-1..1], j <- [-1..1], (i,j) /= (0,0)]
  where
    inBounds (x', y') = x' >= fst lo && x' <= fst hi && y' >= snd lo && y' <= snd hi
