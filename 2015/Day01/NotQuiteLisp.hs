import Data.List (unfoldr)
import Data.Maybe (listToMaybe)

main :: IO ()
main = do
    xs <- readFile "2015/Day01/day01.txt"
    let xs' = map (\ c -> if c == '(' then 1 else -1) xs
        xs'' = unfoldr (\ (sum, xs) -> let sum' = head xs + sum in if null xs then Nothing else Just (sum', (sum', tail xs))) (0, xs')
    print $ last xs''
    print $ (+1) . length . takeWhile (/= -1) $ xs''