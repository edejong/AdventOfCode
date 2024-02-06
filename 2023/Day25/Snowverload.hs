import           Data.Graph.Inductive
import           Data.List            (nub)
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

genMincuts :: (RandomGen gen, Semigroup a, DynGraph g) => g a b -> gen -> [g a b]
genMincuts gr gen = let (gs, gen') = fastMincut gr gen in gs ++ genMincuts gr gen'

{-|Karger–Stein algorithm (Minimum Cut).

This implementation does not return the minimum cut between the two attempts,
instead it returns all cuts. This is to prevent repeated calls to "size"
which is a bottleneck. The desired cut can be determined by the caller.

See https://en.wikipedia.org/wiki/Karger%27s_algorithm#Karger–Stein_algorithm
-}
fastMincut :: (RandomGen gen, Semigroup a, DynGraph g) => g a b -> gen -> ([g a b], gen)
fastMincut gr gen
  | n <= 6 = let (gr', gen') = contract 2 gr gen in ([gr'], gen')
  | otherwise =
    let (g1, gen1) = uncurry fastMincut . contract t gr $ gen
        (g2, gen2) = uncurry fastMincut . contract t gr $ gen1
     in (g1 ++ g2, gen2)
  where
    n = noNodes gr
    t = ceiling (1 + (fromIntegral n / sqrt 2) :: Double)

{-|Karger's algorithm (Minimum Cut).

See https://en.wikipedia.org/wiki/Karger%27s_algorithm
-}
contract :: (RandomGen gen, Semigroup a, DynGraph g) => Int -> g a b -> gen -> (g a b, gen)
contract t gr gen
  | noNodes gr <= t = (gr, gen)
  | otherwise =
    let es        = edges gr
        (i, gen') = randomR (0, length es - 1) gen
        (n1, n2)  = es !! i
        (Just (pre1,_,lab1,suc1), gr1) = match n1 gr
        (Just (pre2,_,lab2,suc2), gr2) = match n2 gr1
        es'       = filter ((/=n2) . snd) . concat $ [pre1,pre2,suc1,suc2]
        ctx       = (es', n2, lab1 <> lab2, [])
        gr3       = insert ctx gr2
     in contract t gr3 gen'
