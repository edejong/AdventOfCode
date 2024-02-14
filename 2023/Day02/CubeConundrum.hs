{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
import Text.Parsec
import Text.Parsec.String

main :: IO ()
main = do
    Right gs <- mapM (parse game "") . lines <$> readFile "2023/Day02/day02.txt"
    let possible (_, hs) = and [all (uncurry (<=)) $ zip h [12,13,14] | h <- hs]
    print $ sum . map fst . filter possible $ gs
    print $ sum . map ((product . foldr (zipWith max) [0,0,0]) . snd) $ gs

game :: Parser (Integer, [[Integer]])
game = string "Game " *> ((,) <$> (int <* string ": ") <*> sepBy1 hand (string "; "))
  where
    hand = foldr (zipWith (+)) [0,0,0] <$> sepBy1 clr (string ", ")
    clr = (\i c -> case c of { "red" -> [i,0,0]; "green" -> [0,i,0]; "blue" -> [0,0,i]})
        <$> int <* spaces <*> (string "red" <|> string "green" <|> string "blue")
    int = read <$> many1 digit
