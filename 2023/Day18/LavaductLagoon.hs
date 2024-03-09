{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

import           Data.List (uncons, unfoldr)
import           Numeric   (readHex)

main :: IO ()
main = do
  xss <- map parseLine . lines <$> readFile "2023/Day18/day18.txt"
  print $ pathArea $ unfoldr f (map fst xss, (0,0))
  print $ pathArea $ unfoldr f (map snd xss, (0,0))
  where
    f :: ([Instruction], Point) -> Maybe (Point, ([Instruction], Point))
    f (xs, pos) = (\(x,xs') -> let p = dig x pos in (pos, (xs', p))) <$> uncons xs

type Instruction = (Char, Integer)

parseLine :: String -> (Instruction, Instruction)
parseLine = (\[a, b, c] -> ((head a, read b), parseClr c)) . words
  where
    parseClr s = let n = fst . head . readHex . take 5 . drop 2 $ s
                  in (dir $ s!!7, n)
    dir = \case {'0' -> 'R'; '1' -> 'D'; '2' -> 'L'; '3' -> 'U'}

type Point = (Integer, Integer)

dig :: Instruction -> Point -> Point
dig (dir, n) (r, c) = (\case 'L' -> (r, c - n); 'R' -> (r, c + n); 'U' -> (r - n, c); 'D' -> (r + n, c)) dir

{- Add half the path length to the surface area of the polygon -}
pathArea :: [Point] -> Integer
pathArea xs =
    let pathLen = sum $ zipWith manhattanDist xs (tail . cycle $ xs)
     in abs (polygonArea xs) + (pathLen `div` 2) + 1

manhattanDist :: Num a => (a, a) -> (a, a) -> a
manhattanDist (a1,a2) (b1,b2) = abs (b1 - a1) + abs (b2 - a2)

polygonArea :: Integral c => [(c, c)] -> c
polygonArea xs = (`div` 2) . sum $ zipWith (\(x1,y1) (x2,y2) -> x1 * y2 - y1 * x2) xs (tail . cycle $ xs)
