import           Data.List (nub)

main :: IO ()
main = do
    xs <- readFile "2022/Day6/day06.txt"
    print (findUnique 4 xs 0, findUnique 14 xs 0)
  where
    findUnique len xs i
        | length (nub . take len $ xs) == len = i + len
        | otherwise = findUnique len (tail xs) (i+1)
