import           Data.Graph.Inductive
import           Data.List            (nub)
import           Mincut               (genMincuts)
import           System.Random

main :: IO ()
main = do
  xs <- lines <$> readFile "2023/Day25/day25.txt"
  let (graph, nodeMap) = buildGraph . map (words . filter (`notElem` ":")) $ xs

  -- Also easily determined by looking at day25.svg.
  let edges' = labEdges . head . filter ((<=3) . size) . genMincuts graph $ mkStdGen 25
      n = fst . mkNode_ nodeMap
      es = concat [[(n a, n b), (n b, n a)] | (_,_,(a, b)) <- edges']
      cs = components . delEdges es $ graph

  print $ product $ map length cs

buildGraph :: [[String]] -> (Gr String (String, String), NodeMap String)
buildGraph xs =
  let nodes' = nub . concat $ xs
      edges' = [(a, b, (a, b)) | (a : bs) <- xs, b <- bs]
   in mkMapGraph nodes' edges'
