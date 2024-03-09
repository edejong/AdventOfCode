{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
import           Data.List     (find, sort)
import           Data.Maybe    (isJust)
import           Data.Sequence (Seq (Empty, (:<|), (:|>)), ViewL ((:<)),
                                ViewR ((:>)), fromList, viewl, viewr, (<|),
                                (|>))

main :: IO ()
main = do
    xs <- sort . map (read @Int) . lines <$> readFile "2020/data/day01.txt"
    print $ findSum 2020 . fromList $ xs
    print $ find isJust $ fmap (\k -> fmap (k*) $ findSum (2020-k) . fromList $ xs) xs

findSum :: (Ord p, Num p) => p -> Seq p -> Maybe p
findSum _ (_:<|Empty) = Nothing
findSum k (x :<| (xs :|> y))
  | x + y < k = findSum k (xs |> y)
  | x + y > k = findSum k (x <| xs)
  | otherwise = Just (x * y)

subLists :: [a] -> [[a]]
subLists []     = []
subLists (x:xs) = xs : map (x:) (subLists xs)
