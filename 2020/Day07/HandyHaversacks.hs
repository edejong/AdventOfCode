{-# LANGUAGE TupleSections #-}
module Day07.HandyHaversacks where
import Text.ParserCombinators.Parsec
import Text.Parsec (endOfLine)
import Text.Parsec.Token (GenTokenParser(whiteSpace))
import Data.Map ( Map, (!), empty, fromList, insertWith, toList )
import Data.Graph (graphFromEdges, dfs)
import Data.Maybe (fromJust)

inputFile = endBy line endOfLine
line = (,) <$> (bagDesc <* (spaces >> string "bags contain" >> spaces)) <*> bagContents <* char '.'
bagDesc = many1 letter <> string " " <> many1 letter
bagContents = [] <$ string "no other bags" <|> sepBy innerBag (string ", ")
innerBag = (\n bag -> (read n::Int, bag)) <$> (many digit <* spaces) <*> (bagDesc <* many (noneOf ",."))

main :: IO ()
main = do
  result <- parseFromFile inputFile "2020/data/day07.txt"
  rules <- case result of
               Left err -> print err >> return []
               Right res -> return res
  let bags = map fst rules

  -- Part 1
  let bags' = map ((,[]) . fst) rules ++ [(outer, [inner]) | (inner, ys) <- rules, (_, outer) <- ys]
  let edges = toList . foldr (\(k, v) m -> insertWith (++) k v m) empty $ bags'
  let (graph, _, vertexFromKey) = graphFromEdges [(k, k, v) | (k, v) <- edges]
  print $ subtract 1 . length . head . dfs graph $ [fromJust $ vertexFromKey "shiny gold"]

  -- Part 2
  print $ (`countNested` "shiny gold") . fromList $ rules

countNested :: Map String [(Int, String)] -> String -> Int
countNested m bag = case m ! bag of
  [] -> 0
  xs -> sum . map (\(n, bag') -> n + n * countNested m bag') $ xs