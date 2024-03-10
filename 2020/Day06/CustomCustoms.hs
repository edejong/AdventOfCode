import           Data.List       (nub)
import           Data.List.Split (splitOn)
import           Data.Map        (fromListWith, toList)

main :: IO ()
main = do
  xs <- map (splitOn "\n") . splitOn "\n\n" <$> readFile "2020/Day06/day06.txt"
  print (sum . map f1 $ xs, sum . map f2 $ xs)
  where
    f1 = length . nub . concat
    f2 xs = length . map fst . filter (\(_, n) -> n == length xs) . toList . fromListWith (+) . concatMap (map (,1)) $ xs
