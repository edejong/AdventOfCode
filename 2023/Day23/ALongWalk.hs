{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

import Data.Function.Memoize (memoize)
import Data.Graph.Inductive
import Data.List (foldl', nub)
import Data.Maybe (fromJust)
import GHC.Arr ( inRange, (!), assocs, bounds, listArray, Array, accumArray )
import qualified GHC.Bits as Bits

type BitSet = Int

member :: Int -> BitSet -> Bool
member = flip Bits.testBit

notMember :: Int -> BitSet -> Bool
notMember x = not . member x

ins :: Int -> BitSet -> BitSet
ins x s = Bits.setBit s x

main :: IO ()
main = do
    xss <- lines <$> readFile "2023/Day23/day23.txt"
    let (nRows, nCols) = (length xss, length . head $ xss)
    let bnds = ((1, 1), (nRows, nCols))
    let arr = listArray bnds (concat xss)

    let start = (1,2)
        dest = (nRows,nCols-1)

    -- PART1
    let (graph1, nodeMap1) = buildGraph arr
    let startNode1 = mkNode_ nodeMap1 start
    let destNode1 = mkNode_ nodeMap1 dest
    let graph1' = collapse graph1
    print $ longestPath graph1' (fst startNode1) (fst destNode1)

    -- PART2
    let arr2 = accumArray (\_ b -> b) '?' (bounds arr) $ (map (\(i,c) -> if c `elem` "<^v>" then (i,'.') else (i,c)) . assocs) arr ++ [(start,'v'), (dest, 'v')]
    let (graph2, _) = buildGraph arr2
    let (graph2', nodeMap2) = (rebuildGraph . collapse) graph2
    let startNode2 = mkNode_ nodeMap2 start
    let destNode2 = mkNode_ nodeMap2 dest
    let graph2'' = fixEnd (fst destNode2) . fixStart (fst startNode2) $ graph2'
    print $ longestPath graph2'' (fst startNode2) (fst destNode2)

fixStart :: Node -> Gr Point Int -> Gr Point Int
fixStart start graph =
  let ns = iterate (concatMap (suc graph)) [start]
      es = [(n2, n1) | n1 <- ns !! 1, n2 <- ns !! 2]
   in delEdges es graph

fixEnd :: Node -> Gr Point Int -> Gr Point Int
fixEnd end graph =
  let ns = iterate (concatMap (pre graph)) [end]
      es = [(n1, n2) | n1 <- ns !! 1, n2 <- ns !! 2]
   in delEdges es graph

buildGraph :: Array (Int, Int) Char -> (Gr Point Int, NodeMap (Int, Int))
buildGraph arr =
    let nodeList = [i | (i,c) <- assocs arr, c `elem` ".<^v>"]
        edgeList = [(n1, n2, 1) | n1 <- nodeList, n2 <- nbrs arr n1]
     in mkMapGraph nodeList edgeList

rebuildGraph :: Gr Point Int -> (Gr Point Int, NodeMap (Int, Int))
rebuildGraph g =
  let nodeLab = fromJust . lab g
      nodeList = [nodeLab n | (n1, n2) <- edges g, n <- [n1,n2]]
      edgeList = [(nodeLab n1, nodeLab n2, w) | (n1, n2, w) <- labEdges g]
   in mkMapGraph nodeList edgeList

longestPath :: Gr Point Int -> Node -> Node -> Int
longestPath graph start dest = longestPath' 0 start
  where
    longestPath' visited node
      | node == dest = 0
      | otherwise =
        let visited' =
              ins node visited
            foo = filter (\(n,_) -> n `notMember` visited) . lsuc'' $ node
            bar = (minBound :) . map (\(n,w) -> w + longestPath' visited' n) $ foo
         in maximum bar
    lsuc'' = memoize (lsuc' . context graph)

collapse :: Gr Point Int -> Gr Point Int
collapse graph = foldl' (flip tryRemoveNode) graph (nodes graph)
  where
    tryRemoveNode n g
      | numNbrs ctx == 2 =
         let es = [(n1, n2, w1+w2) | (n1,w1) <- lpre' ctx, (n2,w2) <- lsuc' ctx, n1 /= n2]
          in insEdges es g'
      | otherwise = insert ctx g'
      where
        (Just ctx, g') = match n g
    numNbrs = length . nub . neighbors'

type Point = (Int, Int)

nbrs :: Array Point Char -> Point -> [Point]
nbrs arr p@(row,col) =
    let ps = case arr ! p of
                '.' -> concat [up,down,left,right]
                '^' -> up
                'v' -> down
                '<' -> left
                '>' -> right
                _ -> []
     in filter (inRange (bounds arr)) ps
  where
    up = filter ((`elem` ".<^>") . (arr !)) . filter (inRange (bounds arr)) $ [(row-1,col)]
    down = filter ((`elem` ".<v>") . (arr !)) . filter (inRange (bounds arr)) $ [(row+1,col)]
    left = filter ((`elem` ".<^v") . (arr !)) . filter (inRange (bounds arr)) $ [(row,col-1)]
    right = filter ((`elem` ".^v>") . (arr !)) . filter (inRange (bounds arr)) $ [(row,col+1)]
