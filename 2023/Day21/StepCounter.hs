import           Data.Set (Set)
import qualified Data.Set as Set
import           GHC.Arr  (Array, (!))
import qualified GHC.Arr  as Arr
import           Linear   (V2 (V2), (!*))

rot45 :: V2 (V2 Int)
rot45 = V2 (V2 1 1) (V2 (-1) 1)

main :: IO ()
main = do
    xss <- grow 5 . lines <$> readFile "2023/Day21/day21.txt"
    let nRows = length xss
    let hSize = nRows `div` 2
    let arr = Arr.listArray (V2 (-hSize) (-hSize), V2 hSize hSize) . concat $ xss
    let odds = filter (odd . sum)
    let evens = filter (even . sum)

    let arr' = Arr.amap (/= '#') arr :: Array (V2 Int) Bool

    print $ length $ evens . Set.toList . dfs arr' $ 64

    let sq = fmap (`div` 131) . (+ V2 65 65) . (rot45 !*)
    let getDiamond (r, c) = filter ((== V2 r c) . sq)

    let n = 2 -- (5x5)

    let ps = Set.toList . dfs arr' $ 65+n*131

    let d1o = length $ odds (getDiamond (0,0) ps)
    let d1e = length $ evens (getDiamond (0,0) ps)
    let d2o = length $ odds (getDiamond (1,0) ps)
    let d2e = length $ evens (getDiamond (1,0) ps)
    let d1o' = length $ concatMap (odds . (`getDiamond` ps)) [(-n,n), (n,n), (n,-n), (-n,-n)] -- N E S W
    let d1o'' = length $ concatMap (odds . (`getDiamond` ps)) [(-n,0), (0,-n), (0,n), (n,0)] -- NW SW NE SE
    let d2o''a = length $ concatMap (odds . (`getDiamond` ps)) [(-n,-1), (n,1)] -- NW SE
    let d2e''b = length $ concatMap (odds . (`getDiamond` ps)) [(-1,-n), (1,n)] -- SW NE

    let calc m = (m-1)^(2::Int) * d1o + m^(2::Int) * d1e + m * (m-1) * (d2o + d2e) + d1o' + (m-1) * d1o'' + m * (d2o''a + d2e''b)

    print $ calc $ (26501365 - 65) `div` 131

dfs :: Array (V2 Int) Bool -> Int -> Set (V2 Int)
dfs arr numSteps =
    uncurry Set.union . (!!numSteps) . iterate (dfsStep arr) $ (Set.empty, Set.singleton (V2 0 0))
  where
    dfsStep arr' (visited, queue) =
        let visited' = Set.union visited queue
            queue' = Set.difference (Set.fromList . concatMap (nbrs arr') . Set.toList $ queue) visited'
        in (visited', queue')

grow :: Int -> [String] -> [String]
grow n = concat . replicate n . map (concat . replicate n)

nbrs :: Array (V2 Int) Bool -> V2 Int -> [V2 Int]
nbrs arr (V2 row col) =
    let ps = map (uncurry V2) [(row-1,col),(row+1,col),(row,col-1),(row,col+1)]
     in [p | p <- ps, arr ! p]
