module Day15.Chiton where


-- TODO: Performance of this one is atrocious for Part 2

import Control.Monad.ST
import Data.Array.ST
import Data.Array (Array, bounds, listArray, (!))
import Data.Array.Base (STUArray(STUArray), unsafeFreeze)
import Data.Char (digitToInt)
import qualified Data.Heap as H
import Data.Heap (MinHeap)
import Data.List (minimumBy, (\\), unfoldr)
import Data.Function (on)

type Point = (Int, Int)
type Grid = Array Point Int
type MinQueue = MinHeap (Int, Point)

dijkstra :: Grid -> Array Point Point
dijkstra grid = runSTArray $ do
    let bnds = bounds grid

    prev <- newArray bnds (-1,-1) :: ST s (STArray s Point Point)
    dist <- newArray bnds (maxBound::Int) :: ST s (STUArray s Point Int)
    writeArray dist (0,0) 0

    let q = range bnds
    -- q = H.fromList $ (0, (0, 0)) : (tail . map (maxBound::Int,) . range . bounds $ grid)
    dijkstraIt grid dist prev q

    return prev

dijkstraIt :: Grid -> STUArray s Point Int -> STArray s Point Point -> [Point] -> ST s ()
dijkstraIt grid dist prev [] = return ()
dijkstraIt grid dist prev q = do
    let bnds = bounds grid

    u <- minElement dist q
    let q' = q \\ [u]

    dist_u <- readArray dist u
    -- let vs = filter (`elem` q') . neighbours8 bnds $ u
    let vs = neighbours8 bnds $ u
    let vs_alt = map ((+dist_u) . (grid !)) vs
    dist_vs <- readArr dist vs
    let tmp = filter (\(v,alt,dist_v) -> alt < dist_v) $ zip3 vs vs_alt dist_vs

    writeArr dist $ map (\(v,alt,dist_v) -> (v,alt)) tmp
    writeArr' prev $ map (\(v,alt,dist_v) -> (v,u)) tmp

    dijkstraIt grid dist prev q'

readArr :: (Ix i) => STUArray s i Int -> [i] -> ST s [Int]
readArr arr [] = return []
readArr arr (i:is) = do
    rest <- readArr arr is
    x <- readArray arr i
    return (x:rest)

writeArr :: (Ix i) => STUArray s i Int -> [(i, Int)] -> ST s ()
writeArr arr [] = return ()
writeArr arr ((i, x):xs) = do
    writeArray arr i x
    writeArr arr xs

writeArr' :: (Ix i) => STArray s i Point -> [(i, Point)] -> ST s ()
writeArr' arr [] = return ()
writeArr' arr ((i, x):xs) = do
    writeArray arr i x
    writeArr' arr xs

minElement :: (Ix i) => STUArray s i Int -> [i] -> ST s i
minElement arr [i] = do return i
minElement arr (i:is) = do
    j <- minElement arr is
    a <- readArray arr i
    b <- readArray arr j
    return (if a < b then i else j)
minElement arr [] = error "Empty list of indices"

neighbours8 :: (Point, Point) -> Point -> [Point]
neighbours8 (lo, hi) (x, y) = filter inBounds [(x -1, y), (x + 1, y), (x, y -1), (x, y + 1)]
  where
    inBounds (x', y') = x' >= fst lo && x' <= fst hi && y' >= snd lo && y' <= snd hi

solve :: [[Int]] -> Int
solve xs =
    let bnds  = ((0, 0), (length xs - 1, (length . head) xs - 1))
        grid  = listArray bnds $ concat xs
        prev  = dijkstra grid
        route = init $ snd bnds : unfoldr (\c -> if c == (0,0) then Nothing else Just (prev ! c, prev ! c)) (snd bnds)
    in sum . map (grid !) $ route

main :: IO ()
main = do
    xs <- map (map digitToInt) . lines <$> readFile "2021/data/day15.txt"
    print $ solve xs

    let xs' = [[((x-1)+k+p) `mod` 9 + 1| k <- [0..1], x <- line] | p <- [0..0], line <- xs]
    print $ solve xs'

    -- let xs' = [[((x-1)+k+p) `mod` 9 + 1| k <- [0..4], x <- line] | p <- [0..4], line <- xs]
    -- print $ solve xs'