import Data.Char (isDigit)
import Text.Parsec

main :: IO ()
main = do
    xs <- lines <$> readFile "2023/Day01/day01.txt"
    print $ sum . map ((\s -> read @Int [head s, last s]) . filter isDigit) $ xs
    print $ sum <$> mapM (\s -> read @Int <$> sequence [parse firstNum "" s, parse lastNum "" s]) xs
  where
    firstNum = digit <* many alphaNum <|> try (num <* many alphaNum) <|> alphaNum *> firstNum
    lastNum = try (alphaNum *> lastNum) <|> digit <* many alphaNum <|> num <* many alphaNum
    num = choice . zipWith (\i n -> i <$ string' n) ['1' ..] $ ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]
