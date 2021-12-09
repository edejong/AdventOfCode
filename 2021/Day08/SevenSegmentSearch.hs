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


--  0000
-- 1    2
-- 1    2
--  3333
-- 4    5
-- 4    5
--  6666

-- [
--       [0, 1, 2, 4, 5, 6]    -- 0
--     , [2, 5]                -- 1
--     , [0, 2, 3, 4, 6]       -- 2
--     , [0, 2, 3, 5, 6]       -- 3
--     , [1, 2, 3, 5]          -- 4
--     , [0, 1, 3, 5, 6]       -- 5
--     , [0, 1, 3, 4, 5, 6]    -- 6
--     , [0, 2, 5]             -- 7
--     , [0, 1, 2, 3, 4, 5, 6] -- 8
--     , [0, 1, 2, 3, 5, 6]    -- 9
-- ]

-- acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf
-- check (x:xs) nums chars
-- check1 "ab" [] [_,_,a,_,_,b,_] = [check [1,"ab"] [_,_,a,_,_,b,_]
-- check1 "ab" [] [_,_,b,_,_,a,_] = [check [1,"ab"] [_,_,b,_,_,a,_]
-- check1 "ab" [] [_,_,a,_,_,_,_] = [check [1,"ab"] [_,_,a,_,_,b,_]
-- check1 "ab" [] [_,_,b,_,_,_,_] = [check [1,"ab"] [_,_,b,_,_,a,_]
-- check1 "ab" [] [_,_,_,_,_,a,_] = [check [1,"ab"] [_,_,a,_,_,b,_]
-- check1 "ab" [] [_,_,_,_,_,b,_] = [check [1,"ab"] [_,_,b,_,_,a,_]
-- check1 "ab" [] [_,_,_,_,_,_,_] = [check [1,"ab"] [_,_,a,_,_,b,_], check [1,"ab"] [_,_,b,_,_,a,_]]

-- [_,_,n2,_,_,n5,_], ["ab", "ba"]


-- >>> tmp "c.a..b." [2,5] "ab" -- Just "c.a..b."
-- >>> tmp "c.a...." [2,5] "ab" -- Just "c.a..b."
-- >>> tmp "c....b." [2,5] "ab" -- Just "c.a..b."
-- >>> tmp "c......" [2,5] "ab" -- Just "c.a..b."
-- >>> tmp "c.x...." [2,5] "ab" -- Nothing
-- >>> tmp "c....x." [2,5] "ab" -- Nothing
-- Variable not in scope: tmp :: [Char] -> [a0] -> [Char] -> t

-- >>> zip [2,5] "ab"
-- [(2,'a'),(5,'b')]

-- tmp xs is ys = let tmp = 

    

-- atIndices :: [Int] -> [b] -> [b]
-- atIndices is xs = map (xs !!) is
