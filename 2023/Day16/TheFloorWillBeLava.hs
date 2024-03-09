import           Data.Set (Set)
import qualified Data.Set as S
import           GHC.Arr

-- Pretty brute force solution; doesn't use the facts that most of the routes
-- massively overlap.
--
-- Idea: Strongly connected component analyses. Each component has the same
--         number of visited nodes everywhere.

main :: IO ()
main = do
  xss <- lines <$> readFile "2023/Day16/day16.txt"
  let (nRows, nCols) = (length xss, length (head xss))
  let arr = listArray ((1, 1), (nRows, nCols)) (concat xss)
  let route = dfs arr ((1, 1), E) S.empty
  print $ S.size . S.map fst $ route

  let routesE = maximum $ map (\row -> S.size . S.map fst . dfs arr ((row, 1), E) $ S.empty) [1 .. nRows]
  let routesW = maximum $ map (\row -> S.size . S.map fst . dfs arr ((row, nCols), W) $ S.empty) [1 .. nRows]
  let routesS = maximum $ map (\col -> S.size . S.map fst . dfs arr ((1, col), S) $ S.empty) [1 .. nCols]
  let routesN = maximum $ map (\col -> S.size . S.map fst . dfs arr ((nRows, col), N) $ S.empty) [1 .. nCols]
  print $ maximum [routesE, routesW, routesS, routesN]

type Point = (Int, Int)
data Dir = N | S | E | W deriving (Eq, Ord, Show)

dfs :: Array Point Char -> (Point, Dir) -> Set (Point, Dir) -> Set (Point, Dir)
dfs m n@(pos, dir) visited
  | not . inRange (bounds m) $ pos = visited
  | n `elem` visited = visited
  | otherwise =
      let as = move pos (m ! pos) dir
       in foldr (dfs m) visited' as
  where
    visited' = S.insert n visited

move :: Point -> Char -> Dir -> [(Point, Dir)]
move pos '|' E  = [movePos pos N, movePos pos S]
move pos '|' W  = [movePos pos N, movePos pos S]
move pos '-' S  = [movePos pos E, movePos pos W]
move pos '-' N  = [movePos pos E, movePos pos W]
move pos '/' N  = [movePos pos E]
move pos '/' E  = [movePos pos N]
move pos '/' S  = [movePos pos W]
move pos '/' W  = [movePos pos S]
move pos '\\' N = [movePos pos W]
move pos '\\' W = [movePos pos N]
move pos '\\' E = [movePos pos S]
move pos '\\' S = [movePos pos E]
move pos _ dir  = [movePos pos dir]

movePos :: Point -> Dir -> (Point, Dir)
movePos (row, col) N = ((row - 1, col), N)
movePos (row, col) S = ((row + 1, col), S)
movePos (row, col) E = ((row, col + 1), E)
movePos (row, col) W = ((row, col - 1), W)
