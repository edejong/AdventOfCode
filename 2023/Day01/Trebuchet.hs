{-# LANGUAGE TypeApplications #-}

module Day01.Trebuchet where

import Control.Monad
import Data.Char (isDigit)
import Text.Parsec
import Text.Parsec.String

main :: IO ()
main = do
  xs <- lines <$> readFile "2023/data/day01.txt"
  print $ sum . map ((\s -> read @Int [head s, last s]) . filter isDigit) $ xs
  print $ sum <$> mapM (\s -> read @Int <$> sequence [parse firstNum "" s, parse lastNum "" s]) xs

firstNum = digit <* many alphaNum <|> try (num <* many alphaNum) <|> alphaNum *> firstNum
lastNum = try (alphaNum *> lastNum) <|> digit <* many alphaNum <|> num <* many alphaNum
num = choice . zipWith (\i n -> i <$ string' n) ['1' ..] $ ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]
