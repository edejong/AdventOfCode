module Day02.CubeConundrum where

import Text.Parsec
import Text.Parsec.String

main :: IO ()
main = do
  Right gs <- mapM (parse game "") . lines <$> readFile "2023/data/day02.txt"
  let possible (n, hs) = and [all (uncurry (<=)) $ zip h [12,13,14] | h <- hs]
  print $ sum . map fst . filter possible $ gs
  print $ sum . map ((product . foldr (zipWith max) [0,0,0]) . snd) $ gs

game = string "Game " *> ((,) <$> (int <* string ": ") <*> sepBy1 hand (string "; "))
hand = foldr (zipWith (+)) [0,0,0] <$> sepBy1 clr (string ", ")
clr = (\i c -> case c of { "red" -> [i,0,0]; "green" -> [0,i,0]; "blue" -> [0,0,i]})
    <$> int <* spaces <*> (string "red" <|> string "green" <|> string "blue")
int = read <$> many1 digit
