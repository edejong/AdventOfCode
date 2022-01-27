module Day08.SevenSegmentSearch where
import Data.Map (fromList, Map, (!))
import Data.List.Split (splitOn)
import Data.List (sort, foldl', (\\), sortOn)

main :: IO ()
main = do
    xs <- map (map (splitOn " ") . splitOn " | ") . lines <$> readFile "2021/data/day08.txt"
    print $ length . filter (\s -> length s `elem` [2,3,4,7]) . concatMap (!!1) $ xs
    print $ sum . map decode $ xs

decode :: [[String]] -> Int
decode input =
    let [xs, ys] = map (map sort) input;
        m = makeNumbersMap xs
    in foldl' (\result k -> result * 10 + k) 0 . map (m !) $ ys

makeNumbersMap :: [String] -> Map String Int
makeNumbersMap xs =
  let [n1, n4, n7, n8] = [head . filterLen k $ xs | k <- [2, 4, 3, 7]]
      n3 = head . filter (\s -> length (s \\ n1) == 3) . filterLen 5 $ xs
      [n5, n2] = sortOn (\s -> length (s \\ n4)) . filterLen 5 $ (xs\\[n3])
      [n9, n0, n6] = sortOn (\s -> [length (s\\n) | n <- [n4,n7]]) . filterLen 6 $ xs
   in fromList . zip [n0, n1, n2, n3, n4, n5, n6, n7, n8, n9] $ [0..]
  where filterLen n = filter (\s -> length s == n)