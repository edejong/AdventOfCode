import           Data.List       (sort)
import           Data.List.Split (splitOn)

main :: IO ()
main = do
  xs <- reverse . sort . map (sum . map (read @Int) . words) . splitOn "\n\n" <$> readFile "2022/Day01/day01.txt"
  print (sum . take 1 $ xs, sum . take 3 $ xs)
