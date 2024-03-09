{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
import           Data.Bifunctor       (second)
import           Data.Graph.Inductive
import           Data.List            (foldl', inits, tails, transpose)
import           Data.Map             (fromList, (!))
import           Data.Maybe           (fromJust)

main :: IO ()
main = do
  xss <- map (map (\c -> read [c])) . lines <$> readFile "2023/Day17/day17.txt"
  print $ (\(g, start, end) -> shortestPath g start end) $ genGraph 1 3 xss
  print $ (\(g, start, end) -> shortestPath g start end) $ genGraph 4 10 xss

type Point = (Int, Int)
data Dir = H | V deriving (Enum, Eq, Ord, Show)
type NodeLabel = (Point, Dir)

genGraph :: Int -> Int -> [[Int]] -> (Gr NodeLabel Int, Node, Node)
genGraph minSteps maxSteps xss =
  let (nRows, nCols) = (length xss, length . head $ xss)
      xss' = zipWith (\i xs -> zipWith (\j x -> ((i, j), x)) [0 ..] xs) [0 ..] xss
      nodeLabels = [((r, c), dir) | r <- [0 .. (nRows - 1)], c <- [0 .. (nCols - 1)], dir <- [H, V]] :: [NodeLabel]
      (nodes', nodeMap) = mkNodes new nodeLabels
      edges' = fromJust . mkEdges nodeMap $ genEdges minSteps maxSteps xss'
      ([start, end], nodeMap') = mkNodes nodeMap [((nRows, nCols), H), ((nRows, nCols), V)]
      startEdges = fromJust $ mkEdges nodeMap' [(snd start, ((0, 0), d), 0) | d <- [H, V]]
      endEdges = fromJust $ mkEdges nodeMap' [(((nRows - 1, nCols - 1), d), snd end, 0) | d <- [H, V]]
      nodes'' = nodes' ++ [start, end]
      edges'' = edges' ++ startEdges ++ endEdges
      g = mkGraph nodes'' edges''
   in (g, fst start, fst end)

genEdges :: Int -> Int -> [[(Point, Int)]] -> [(NodeLabel, NodeLabel, Int)]
genEdges minSteps maxSteps xss = mkEdges' H V xss ++ mkEdges' V H (transpose xss)
  where
    mkEdges' d1 d2 xss' = map (asEdge d1 d2) . concatMap (concatMap fooo) $ [xss', map reverse xss']
    asEdge dir dir' (a, b) = ((fst a, dir), (fst b, dir'), snd b)
    fooo = concatMap nbrs . init . tails
    nbrs (x : xs) = [(x, f xs') | xs' <- (drop minSteps . inits . take maxSteps) xs]
    f (y : ys) = foldl' (second . (+) . snd) y ys

shortestPath :: Gr NodeLabel Int -> Node -> Node -> Int
shortestPath g start end =
  let route = fromJust $ sp start end g
      edgesMap = fromList [((a, b), x) | (a, b, x) <- labEdges g]
   in sum $ zipWith (curry (edgesMap !)) route (tail route)
