{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
import           Data.Either (lefts, rights)
import           Data.List   (sort)
import           Data.Map    (fromList, member, (!))

main :: IO ()
main = do
  xs <- map (`parse` []) . lines <$> readFile "2021/Day10/day10.txt"
  let (corrupted, incomplete) = (lefts xs, rights xs)
  print $ sum . map score1 $ corrupted
  print $ (!! (length incomplete `div` 2)) . sort . map score2 $ incomplete

parse :: String -> String -> Either Char String
parse [] ys = Right ys
parse (x:xs) ys | x `member` parens = parse xs (parens ! x : ys)
  where parens = fromList . map (\[a, b] -> (a, b)) $ ["()", "[]", "{}", "<>"]
parse (x:xs) (y:ys) | x == y = parse xs ys
parse (x:_) _ = Left x

score1 :: Char -> Integer
score1 = (fromList [(')', 3), (']', 57), ('}', 1197), ('>', 25137)] !)

score2 :: [Char] -> Integer
score2 = foldl (\a b -> a * 5 + (scores ! b)) 0
  where scores = fromList [(')', 1), (']', 2), ('}', 3), ('>', 4)]
