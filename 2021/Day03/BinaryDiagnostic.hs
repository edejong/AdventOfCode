module Day03.BinaryDiagnostic where

main :: IO ()
main = do
  xss <- lines <$> readFile "data/day03.txt"
  let [ga, o2, ep, co2] =
        [(readBin . countAndFilter f1 f2) xss | f1 <- [(<), (>=)], f2 <- [\_ _ -> True, (==)]]
  print (ga * ep, o2 * co2)
  where
    readBin = foldl (\n c -> n * 2 + if c == '0' then 0 else 1) 0

countAndFilter :: (Int -> Int -> Bool) -> (Char -> Char -> Bool) -> [String] -> String
countAndFilter _ _ [xs] = xs
countAndFilter _ _ ([] : _) = []
countAndFilter f1 f2 xss =
  let c = if f1 (bitDiff (map head xss)) 0 then '0' else '1'
   in c : countAndFilter f1 f2 (map tail . filter (\a -> f2 (head a) c) $ xss)
  where
    bitDiff = foldr (\xs a -> a + if xs == '0' then -1 else 1) 0