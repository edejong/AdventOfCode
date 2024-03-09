import           Array                (showArr2D)
import           Data.Char            (digitToInt, intToDigit)
import           Data.Graph.Inductive
import           GHC.Arr              (Array, amap, assocs, bounds, inRange,
                                       indices, listArray, (!))

main :: IO ()
main = do
    xss <- map (map digitToInt) . lines <$> readFile "2021/Day15/day15.txt"
    print $ solve xss
    let xss' = [[((x-1)+k+p) `mod` 9 + 1| k <- [0..4], x <- line] | p <- [0..4], line <- xss]
    print $ solve xss'
  where
    solve xss = do
        let (nRows, nCols) = (length xss, (length . head) xss)
        let arr = listArray ((1,1),(nRows,nCols)) $ concat xss
        let (gr, nodeMap) = buildGraph arr
         in spLength (fst $ mkNode_ nodeMap (1,1)) (fst $ mkNode_ nodeMap (nRows,nCols)) gr

type Point = (Int, Int)

buildGraph :: Array Point Int -> (Gr Point Int, NodeMap Point)
buildGraph arr =
    let nodeList = indices arr
        edgeList = [(i1, i2, arr ! i2) | (i1,_) <- assocs arr, i2 <- nbrs arr i1]
     in mkMapGraph nodeList edgeList

nbrs :: Array Point Int -> Point -> [Point]
nbrs arr (row,col) = filter (inRange (bounds arr)) [(row+1,col), (row-1,col), (row,col+1), (row,col-1)]
