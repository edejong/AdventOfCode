import Data.List (sort, isInfixOf)

main :: IO ()
main = do
    xs <- lines <$> readFile "2015/Day05/day05.txt"
    print $ length . filter isNice1 $ xs
    print $ length . filter isNice2 $ xs

isNice1 :: String -> Bool
isNice1 xs = hasThreeVowels xs && hasRepeat 1 xs && (not . containsAnyOf ["ab","cd","pq","xy"]) xs
  where hasThreeVowels = (>=3) . length . filter (`elem` "aeiou")
        containsAnyOf ys xs = any (`isInfixOf` xs) ys

isNice2 :: Ord a => [a] -> Bool
isNice2 xs = hasPair xs && hasRepeat 2 xs
  where hasPair (x:y:xs) = [x,y] `isInfixOf` xs || hasPair (y:xs)
        hasPair _ = False

hasRepeat :: Eq a => Int -> [a] -> Bool
hasRepeat n xs = or . zipWith (==) xs $ drop n xs
