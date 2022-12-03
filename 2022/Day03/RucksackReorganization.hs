module Day03.RucksackReorganization where
import qualified Data.Set as Set
import Data.Set (fromList)
import Data.Char (isLower, ord)
import Data.List (foldl')
import Data.List.Split (chunksOf)

main :: IO ()
main = do
    xss <- lines <$> readFile "2022/data/day03.txt"
    print (getResult (map split) xss, getResult (chunksOf 3) xss)
  where
    getResult f = sum . map (sum . map priority . intersections) . f
    split xs = let n = length xs `div` 2 in [take n xs, drop n xs]
    intersections = Set.toList . (\xs -> foldl' Set.intersection (head xs) (tail xs)) . map Set.fromList
    priority c = ord c + (if isLower c then 1 - ord 'a' else 27 - ord 'A')
