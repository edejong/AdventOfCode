-- TODO: Performance of this one is atrocious
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
import           Control.Monad.ST
import           Data.Array       (Array, bounds, listArray, (!))
import           Data.Array.ST
import           Data.Char        (digitToInt)
import           Data.Heap        (MinHeap)
import qualified Data.Heap        as H
import           Data.List        (unfoldr)
import           Data.Tuple       (swap)

type Point = (Int, Int)
type Grid = Array Point Int
type MinQueue = MinHeap (Int, Point)

dijkstra :: Grid -> Array Point Point
dijkstra grid = runSTArray $ do
    let bnds = bounds grid

    prev <- newArray bnds (-1,-1) :: ST s (STArray s Point Point)
    dist <- newArray bnds (maxBound::Int) :: ST s (STUArray s Point Int)
    writeArray dist (0,0) 0

    let q = H.fromList $ (0, (0, 0)) : (tail . map (maxBound::Int,) . range . bounds $ grid) :: MinQueue
    dijkstraIt grid dist prev q

    return prev

dijkstraIt :: Grid -> STUArray s Point Int -> STArray s Point Point -> MinQueue -> ST s ()
dijkstraIt _ _ _ q | H.isEmpty q = return ()
dijkstraIt grid dist prev q = do
    let bnds = bounds grid

    let Just ((_,u), q') = H.view q

    dist_u <- readArray dist u
    -- let vs = filter (`elem` q') . neighbours8 bnds $ u
    let vs = neighbours8 bnds u
    let vs_alt = map ((+dist_u) . (grid !)) vs
    dist_vs <- readArr dist vs
    let tmp = filter (\(_,alt,dist_v) -> alt < dist_v) $ zip3 vs vs_alt dist_vs

    let dists = map (\(v,alt,_) -> (v,alt)) tmp
    writeArr dist $ dists
    writeArr' prev $ map (\(v,_,_) -> (v,u)) tmp

    dijkstraIt grid dist prev (updatePrios (map swap dists) q')

updatePrios :: [(Int, Point)] -> MinQueue -> MinQueue
updatePrios prios = H.union q2 . H.filter (\(_, p') -> p' `notElem` tmp)
  where tmp = map snd prios
        q2 = H.fromList prios

readArr :: (Ix i) => STUArray s i Int -> [i] -> ST s [Int]
readArr _ [] = return []
readArr arr (i:is) = do
    rest <- readArr arr is
    x <- readArray arr i
    return (x:rest)

writeArr :: (Ix i) => STUArray s i Int -> [(i, Int)] -> ST s ()
writeArr _ [] = return ()
writeArr arr ((i, x):xs) = do
    writeArray arr i x
    writeArr arr xs

writeArr' :: (Ix i) => STArray s i Point -> [(i, Point)] -> ST s ()
writeArr' _ [] = return ()
writeArr' arr ((i, x):xs) = do
    writeArray arr i x
    writeArr' arr xs

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
    xs <- map (map digitToInt) . lines <$> readFile "2021/Day15/day15.txt"
    print $ solve xs

    -- let xs' = [[((x-1)+k+p) `mod` 9 + 1| k <- [0..1], x <- line] | p <- [0..0], line <- xs]
    -- print $ solve xs'

    let xs' = [[((x-1)+k+p) `mod` 9 + 1| k <- [0..4], x <- line] | p <- [0..4], line <- xs]
    print $ solve xs'
