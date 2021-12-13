module Day12.PassagePathing where
import Data.Map (Map, insertWith, empty, (!))
import qualified Data.Map as Map
import Data.List.Split (splitOn)
import Data.Char (isLower)

type Graph = Map String [String]

main :: IO ()
main = do
    xs <- map (splitOn "-") . lines <$> readFile "2021/data/day12.txt"
    let edges = filter (\(_, b) -> b /= "start") . concatMap (\[a, b] -> [(a, b), (b, a)]) $ xs
        graph = foldr (\(a, b) m -> insertWith (++) a [b] m) empty edges
    print $ countPaths graph ["start"] [] [] False
    print $ countPaths graph ["start"] [] [] True

countPaths :: Graph -> [String] -> [String] -> [String] -> Bool -> Int
countPaths _ [] _ _ _ = 0
countPaths graph (x:xs) path visited twice
  | x == "end"       = rest + 1
  | x `elem` visited = rest + if twice then countPaths graph (graph ! x) (x:path) visited False else 0
  | small            = rest + countPaths graph (graph ! x) (x:path) (x:visited) twice
  | otherwise        = rest + countPaths graph (graph ! x) (x:path)    visited  twice
  where
    small = (isLower . head) x
    rest = countPaths graph xs path visited twice