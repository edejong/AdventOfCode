module Day10.PipeMaze where

import Data.List (unfoldr)
import GHC.Arr (Array, bounds, inRange, (!))
import qualified GHC.Arr as Arr

-- There's an off by one error in part 2. Output is 510 but solution is 511

main :: IO ()
main = do
  xss <- lines <$> readFile "2023/Day10/day10.txt"
  let (w, h) = (length xss, length (head xss))
  let maze = Arr.listArray ((1, 1), (w, h)) (concat xss)
  let start = fst . head . filter ((== 'S') . snd) . Arr.assocs $ maze
  let poly = map fst . head . filter (not . null) . map (\f -> walk maze $ f start) $ [east, west, north, south]
  print $ (length poly + 1) `div` 2
  print $ polygonArea poly - ((length poly - 1) `div` 2)

type Pos = (Int, Int)
type Maze = Array Pos Char
data Dir = N | S | E | W deriving (Show)
type PosDir = (Pos, Dir)

walk :: Maze -> PosDir -> [PosDir]
walk maze posDir = unfoldr f posDir
  where
    f posDir = (\p -> (p, p)) <$> step maze posDir

step :: Maze -> PosDir -> Maybe PosDir
step maze (pos@(row, col), dir) | inRange (bounds maze) pos = next (maze ! pos) dir
  where
    next '|' N = Just $ south pos
    next '|' S = Just $ north pos
    next '-' E = Just $ west pos
    next '-' W = Just $ east pos
    next 'L' N = Just $ east pos
    next 'L' E = Just $ north pos
    next 'J' N = Just $ west pos
    next 'J' W = Just $ north pos
    next '7' S = Just $ west pos
    next '7' W = Just $ south pos
    next 'F' S = Just $ east pos
    next 'F' E = Just $ south pos
    next _ _ = Nothing
step _ _ = Nothing

north (row, col) = ((row - 1, col), S)
south (row, col) = ((row + 1, col), N)
east (row, col) = ((row, col + 1), W)
west (row, col) = ((row, col - 1), E)

polygonArea :: Integral c => [(c, c)] -> c
polygonArea xs = (`div` 2) . abs . sum $ zipWith (\(x1,y1) (x2,y2) -> x1 * y2 - y1 * x2) xs (tail . cycle $ xs)
