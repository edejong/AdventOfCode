module Day05.BinaryBoarding where
import Numeric (readInt)
import Data.List ((\\))

main :: IO ()
main = do
    xs <- map readBin . lines <$> readFile "2020/data/day05.txt"
    print $ maximum xs
    print $ [minimum xs..maximum xs] \\ xs

readBin :: [Char] -> Int
readBin = fst . head . readInt 2 (`elem` "FBLR") (\c -> if c `elem` "FL" then 0 else 1)
