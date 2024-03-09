import           Data.List       (inits, tails, transpose)
import           Data.List.Split (splitOn)

main :: IO ()
main = do
    xss <- map lines . splitOn "\n\n" <$> readFile "2023/Day13/day13.txt"
    print (f 0 xss, f 1 xss)
  where
    f nSmudges = sum . map (\xs -> 100 * (sum . mirrorIndex nSmudges $ xs) + (sum . mirrorIndex nSmudges . transpose $ xs))

mirrorIndex :: Int -> [[Char]] -> [Int]
mirrorIndex nSmudges = map fst . filter snd . zip [1..] . map ((== nSmudges) . uncurry mirrorDiff) . splits
  where
    splits xs = tail . init $ zip (inits xs) (tails xs)
    mirrorDiff xs = sum . zipWith diff (reverse xs)
    diff xs = length . filter id . zipWith (/=) xs
