import           Data.List.NonEmpty (fromList, xor)
import           Data.List.Split    (dropBlanks, dropDelims, oneOf, split)

main :: IO ()
main = do
    xs <- map parse . lines <$> readFile "2020/Day02/day02-test.txt"
    print $ length . filter isValid1 $ xs
    print $ length . filter isValid2 $ xs

parse :: [Char] -> (Int, Int, Char, [Char])
parse = (\[a, b, [c], xs] -> (read a, read b, c, xs)) . split (dropDelims . dropBlanks $ oneOf ":- ")

isValid1 :: Eq c => (Int, Int, c, [c]) -> Bool
isValid1 (a, b, c, xs) = let count = length . filter (==c) $ xs in
    count >= a && count <= b

isValid2 :: Eq a => (Int, Int, a, [a]) -> Bool
isValid2 (a, b, c, xs) = xor . fromList $ [xs !! (a-1) == c, xs !! (b-1) == c]
