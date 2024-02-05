import Data.List (nub)
import Data.Graph.Inductive (components, mkMapGraph, mkNode_, delEdges, undir, Gr, NodeMap, maxFlowgraph, Graph (labEdges), edgeLabel, maxFlow, nodes, context, lab')

main :: IO ()
main = do
    xs <- map (words . filter (`notElem` ":")) . lines <$> readFile "2023/Day25/day25.txt"
    let ns = nub . concat $ xs
    let es = [(n1,n2,1) | (n1:ns) <- xs, n2 <- ns]
    let (graph, nodeMap) = mkMapGraph ns es :: (Gr String Int, NodeMap String)

    -- As determined by just looking at day25.svg.
    -- TODO: https://en.wikipedia.org/wiki/Karger%27s_algorithm
    let edges = [("bbp", "dvr"), ("gtj", "tzj"), ("jzv", "qvq")]
    let es = concat [[(n1, n2), (n2, n1)] | (a, b) <- edges, let [n1, n2] = map (fst . mkNode_ nodeMap) [a, b]]
    let cs = components . delEdges es . undir $ graph
    print $ product $ map length cs
