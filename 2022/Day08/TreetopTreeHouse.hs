{-# LANGUAGE TypeApplications #-}
module Day08.TreetopTreeHouse where
import Data.List (transpose, zipWith4, tails, zip4)

-- TODO: it feels like you should be able to generalise the two parts.

main :: IO ()
main = do
    xs <- map (map ((read @Int) . (: []))) . lines <$> readFile "2022/data/day08.txt"
    print $ sum . map (length . filter (== True)) . isVisible2D $ xs
    print $ maximum . map scenicScore . concat . viewDists2D $ xs
  where
    scenicScore (r, l, d, u) = r * l * d * u

isVisible1D :: [Int] -> [Bool]
isVisible1D xs = zipWith (||) (isVisible1D' (-1) xs) (reverse (isVisible1D' (-1) (reverse xs)))
  where
    isVisible1D' _ [] = []
    isVisible1D' h (x:xs)
        | x > h = True : isVisible1D' x xs
        | otherwise = False : isVisible1D' h xs

isVisible2D :: [[Int]] -> [[Bool]]
isVisible2D xss =
    let as = map isVisible1D xss
        bs = transpose (map isVisible1D (transpose xss))
    in zipWith (zipWith (||)) as bs


viewDists :: [Int] -> [Int]
viewDists = map (\(a:bs) -> viewDist a bs) . init . tails
  where viewDist h = length . takeUntil (>= h)

viewDists2D :: [[Int]] -> [[(Int, Int, Int, Int)]]
viewDists2D xss =
    let distsR = map viewDists xss
        distsL = map (reverse . viewDists . reverse) xss
        distsD = transpose . map viewDists . transpose $ xss
        distsU = transpose . map (reverse . viewDists . reverse) . transpose $ xss
    in zipWith4 zip4 distsR distsL distsD distsU

takeUntil :: (Int -> Bool) -> [Int] -> [Int]
takeUntil f [] = []
takeUntil f (x:xs) = x : (if f x then [] else takeUntil f xs)
