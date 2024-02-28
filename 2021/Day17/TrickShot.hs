import           Data.List                     (find)
import           Data.Maybe                    (fromJust)
import           Text.Parsec.String            (Parser)
import           Text.ParserCombinators.Parsec

main :: IO ()
main = do
    xs <- parseFromFile parser "2021/Day17/day17.txt" >>= either (error . show) pure

    -- Part 1
    let y = abs . fst . snd $ xs
    print $ naturalSum (y-1)

    -- Part 2
    let ymin = (fst . snd) xs
    let ymax = (abs . fst . snd) xs - 1
    let xmin = fromJust . find (\n -> naturalSum n >= (fst . fst $ xs)) $ [0..]
    let xmax = snd . fst $ xs
    print $ length [(x, y) | x <- [xmin..xmax], y <- [ymin..ymax], launch xs (0,0) (x,y)]
  where naturalSum n = n * (n+1) `div` 2

launch r@((x1, x2), (y1, y2)) (x, y) (dx, dy)
    | x >= x1 && x <= x2 && y >= y1 && y <= y2 = True
    | x > x2 || y < y1 = False
    | otherwise = launch r (x + dx, y + dy) (dx - signum dx, dy - 1)

parser :: Parser ((Int, Int), (Int, Int))
parser = (,) <$> (string "target area: x=" *> coord) <*> (string ", y=" *> coord)
  where
    num = read <$> (many1 digit <|> (string "-" <> many1 digit))
    coord = (,) <$> num <*> (string ".." *> num)
