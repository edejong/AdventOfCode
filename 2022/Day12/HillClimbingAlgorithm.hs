import           Data.Char (ord)
import           Data.List (findIndices, nub)
import           Data.Set  (Set, notMember)
import qualified Data.Set  as S

main :: IO ()
main = do
    heightMap <- lines <$> readFile "2022/Day12/day12.txt"
    let bounds = (length (head heightMap), length heightMap)
        coords = findPoints bounds heightMap
        endPoint = head . coords $ 'E'
        tmp = subtract 1 . head . findIndices (\(s,_) -> S.member endPoint s) . iterate (step bounds heightMap)
    print (tmp (S.empty, coords 'S'), tmp (S.empty, coords 'a'))

type Point = (Int, Int)

-- TODO: Reader
findPoints :: Point -> [[Char]] -> Char -> [Point]
findPoints (w,h) heightMap c =
    let coords = [(x,y) | y <- [0..h-1], x <- [0..w-1]]
    in map fst . filter (\(_,c') -> c == c') . zip coords $ concat heightMap

-- TODO: Reader, State
step :: Point -> [[Char]] -> (Set Point, [Point]) -> (Set Point, [Point])
step bounds heightMap (visited, queue) =
    let visited' = S.union visited (S.fromList queue)
        queue' = filter (`notMember` visited) . nub . concatMap (neighbours bounds heightMap) $ queue
    in (visited', queue')

-- TODO: Reader
neighbours :: Point -> [[Char]] -> Point -> [Point]
neighbours (w, h) heightMap (x, y) =
    filter (canMove (x,y)) . filter inBounds $ [(x-1,y),(x+1,y),(x,y-1),(x,y+1)]
  where
    inBounds (x', y') = x' >= 0 && x' < w && y' >= 0 && y' < h
    canMove from to = heightAt to <= heightAt from + 1
    heightAt (x',y') = height ((heightMap !! y') !! x')

height :: Char -> Int
height 'S' = height 'a'
height 'E' = height 'z'
height c   = ord c - ord 'a'
