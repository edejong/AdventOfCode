{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
import Data.List ( nub, transpose )
import Data.Sequence (unfoldl)

main :: IO ()
main = do
    xs <- readFile "2015/Day03/day03.txt"
    print $ length . nub . path xs $ (0, 0)
    print $ length . nub . concatMap (\ inp -> path inp (0, 0)) . deinterleave $ xs

move :: (Num a1, Num a2) => Char -> (a2, a1) -> (a2, a1)
move '^' (x, y) = (x, y+1)
move 'v' (x, y) = (x, y-1)
move '>' (x, y) = (x+1, y)
move '<' (x, y) = (x-1, y)

path :: (Num a1, Num a2) => [Char] -> (a2, a1) -> [(a2, a1)]
path (x:xs) p = let p' = move x p in p : path xs p'
path _ p = [p]

deinterleave :: [a] -> [[a]]
deinterleave xs = transpose . deinterleave' $ xs
  where deinterleave' (x:y:xs) = [x,y] : deinterleave' xs
        deinterleave' _ = []
