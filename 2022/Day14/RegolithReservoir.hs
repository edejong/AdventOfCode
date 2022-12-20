{-# LANGUAGE TypeApplications #-}
module Day14.RegolithReservoir where
import Data.List.Split (splitOn, chunksOf)
import Data.List ( sort, transpose )
import Data.Char (intToDigit)
import Data.Array.IO (IOUArray, MArray (newArray, getBounds), getElems, writeArray, readArray)
import Control.Monad (forM_)

main :: IO ()
main = do
    xs <- map (map (map (read @Int) . splitOn ",") . splitOn " -> ") . lines <$> readFile "2022/data/day14.txt"
    let maxY = maximum . map (!!1) . concat $ xs
    part1 <- solve xs
    part2 <- solve ([[0, maxY + 2], [1000, maxY + 2]]:xs)
    print (part1, part2)

solve :: [[[Int]]] -> IO Int
solve xs = do
    let xs' = (500:) . map (!!0) . concat $ xs
        ys' = (0:) . map (!!1) . concat $ xs
        bounds = ((minimum xs', minimum ys'), (maximum xs', maximum ys'))
    let coords = concatMap asCoords xs
    grid <- newArray bounds '.' :: IO Grid
    forM_ coords (\c -> writeArray grid c '#')
    dropSand grid 0
  where
    dropSand grid count = do
        tmp <- readArray grid (500, 0)
        landed <- tryDrop grid (500, 0)
        if landed && tmp == '.' then dropSand grid (count + 1) else return count

type Point = (Int, Int)

asCoords :: [[Int]] -> [Point]
asCoords xss = concat . zipWith f xss $ tail xss
  where
    f [x1, y1] [x2, y2] = [(x, y) | x <- range' x1 x2, y <- range' y1 y2]
    range' a b = if a < b then [a..b] else [b..a]

type Grid = IOUArray Point Char
data Result = Open | Blocked | OutOfBounds

tryDrop :: Grid -> Point -> IO Bool
tryDrop grid (x,y) = do
    let down = (x,y+1)
        left = (x-1,y+1)
        right = (x+1,y+1)
    tryDrop' grid (x,y) [down, left, right]

tryDrop' :: Grid -> Point -> [Point] -> IO Bool
tryDrop' grid pos [] = do
    writeArray grid pos 'o'
    return True
tryDrop' grid pos (a:as) = do
    bounds <- getBounds grid
    if (not . inBounds bounds) a then
        return False
    else do
        c <- readArray grid a
        if c == '.' then tryDrop grid a else tryDrop' grid pos as
  where
    inBounds ((x1,y1), (x2,y2)) (x, y) = x >= x1 && x <= x2 && y >= y1 && y <= y2

printGrid :: Grid -> IO ()
printGrid grid = do
    elems <- getElems grid
    ((x0,y0),(x1,y1)) <- getBounds grid
    let tmp = unlines . transpose . chunksOf (y1-y0+1) $ elems
    putStr tmp