{-# LANGUAGE TypeApplications #-}
module Day09.RopeBridge where
import Data.List (foldl', unfoldr, nub)

main :: IO ()
main = do
    xs <- map (\ l -> (head l, read @Int . drop 2 $ l)) . lines <$> readFile "2022/data/day09.txt"
    let hs = unfoldr step (xs, (0, 0))
        ts = follow hs
    print (length . nub $ ts !! 1, length . nub $ ts !! 9)
  where
    follow = iterate (tail . scanl followStep (0, 0))

type Point = (Int, Int)
type Step = (Char, Int)

step :: ([Step], Point) -> Maybe (Point, ([Step], Point))
step ([], _) = Nothing
step ((_, 0):xs, p) = step (xs, p)
step ((dir, steps):xs, p) =
    let p' = move dir p
        xs' = (dir, steps - 1):xs
    in Just (p', (xs', p'))

move :: Char -> Point -> Point
move 'R' (x, y) = (x + 1, y)
move 'L' (x, y) = (x - 1, y)
move 'U' (x, y) = (x, y + 1)
move 'D' (x, y) = (x, y - 1)

followStep :: Point -> Point -> Point
followStep t@(tX, tY) (hX, hY)
  | abs dx > 1 || abs dy > 1 = (tX + signum dx, tY + signum dy)
  | otherwise = t
  where (dx, dy) = (hX - tX, hY - tY)
