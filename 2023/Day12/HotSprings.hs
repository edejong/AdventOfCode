{-# LANGUAGE TypeApplications #-}
import Data.List.Split (splitOn)
import Debug.Trace (trace)
import Data.List (intercalate)
import Data.Function.Memoize (memoize2)

main :: IO ()
main = do
    xss <-  map parseLine . lines <$> readFile "2023/Day12/day12.txt"
    print $ sum . map (uncurry possibilities) $ xss
    print $ sum . map (uncurry possibilities . uncurry expand) $ xss
  where
    parseLine xs = let [a,b] = words xs in (a, map (read @Int) . splitOn "," $ b)
    expand xs ys = (intercalate "?" . replicate 5 $ xs, concat . replicate 5 $ ys)

possibilities :: String -> [Int] -> Int
possibilities = memoize2 possibilities'
  where
    possibilities' :: String -> [Int] -> Int
    possibilities' [] _ = 0
    possibilities' xs [] = if '#' `notElem` xs then 1 else 0
    possibilities' ('.':xs) ys = possibilities xs ys
    possibilities' ('?':xs) ys = possibilities ('#':xs) ys + possibilities ('.':xs) ys
    possibilities' xs [y] | length xs == y && '.' `notElem` xs = 1
    possibilities' xs (y:ys)
                | '.' `notElem` as && (null bs || head bs == '.') = possibilities bs' ys
                | '.' `elem` as = 0
                | not (null bs) && head bs == '#' = 0
                | otherwise = possibilities bs' ys
      where
        (as, bs) = splitAt y xs
        bs' = if null bs then bs else '.':tail bs
