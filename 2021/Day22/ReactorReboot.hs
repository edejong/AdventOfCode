import           Data.List          (foldl')
import           Text.Parsec        (char, digit, endOfLine, many1, sepBy1,
                                     string, try, (<|>))
import           Text.Parsec.String (Parser, parseFromFile)

type Range = (Int, Int)
data Cuboid = Cuboid { x::Range, y::Range, z::Range } deriving (Eq, Show)

main :: IO ()
main = do
    steps <- either (error . show) id <$> parseFromFile parser "2021/Day22/day22.txt"
    let part1Bounds = Cuboid (-50, 50) (-50, 50) (-50, 50)
    print $ sum . map volume $ doSteps . filter ((`isInside` part1Bounds) . snd) $ steps
    print $ sum . map volume $ doSteps steps

doSteps :: [(Bool, Cuboid)] -> [Cuboid]
doSteps = foldl' (flip (uncurry doStep)) []
  where doStep swOn c = foldr (\ x' -> (++) (x' `subtractCuboid` c)) ([c | swOn])

------ Cuboid

overlaps :: Cuboid -> Cuboid -> Bool
overlaps c1 c2 = rangesOverlap (x c1) (x c2) && rangesOverlap (y c1) (y c2) && rangesOverlap (z c1) (z c2)
  where rangesOverlap s1 s2 = max (fst s1) (fst s2) <= min (snd s1) (snd s2)

isInside :: Cuboid -> Cuboid -> Bool
isInside (Cuboid (x0, x1) (y0, y1) (z0, z1)) (Cuboid (x2, x3) (y2, y3) (z2, z3)) =
    x0 >= x2 && x1 <= x3 && y0 >= y2 && y1 <= y3 && z0 >= z2 && z1 <= z3

volume :: Cuboid -> Int
volume (Cuboid (x1, x2) (y1, y2) (z1, z2)) = (x2 - x1 + 1) * (y2 - y1 + 1) * (z2 - z1 + 1)

isValidCuboid :: Cuboid -> Bool
isValidCuboid (Cuboid (x1, x2) (y1, y2) (z1, z2)) = x1 <= x2 && y1 <= y2 && z1 <= z2

-- This could be done more elegantly. The idea is to go over each dimension and split
-- off parts of c1 that are outside of c2. The part that overlaps will then get the same
-- treatment in the next dimension and so on.
subtractCuboid :: Cuboid -> Cuboid -> [Cuboid]
subtractCuboid c1 c2 | not (overlaps c1 c2) = [c1]
subtractCuboid (Cuboid (x0, x1) (y0, y1) (z0, z1)) (Cuboid (x2, x3) (y2, y3) (z2, z3)) =
    filter isValidCuboid [xLeft, xRight, yLeft, yRight, zLeft, zRight]
  where
    (x0', x1') = (max x0 x2, min x1 x3)
    (y0', y1') = (max y0 y2, min y1 y3)
    xLeft = Cuboid (x0, x2-1) (y0, y1) (z0, z1)
    xRight = Cuboid (x3+1, x1) (y0, y1) (z0, z1)
    yLeft = Cuboid (x0', x1') (y0, y2-1) (z0, z1)
    yRight = Cuboid (x0', x1') (y3+1, y1) (z0, z1)
    zLeft = Cuboid (x0', x1') (y0', y1') (z0, z2-1)
    zRight = Cuboid (x0', x1') (y0', y1') (z3+1, z1)

------ Parser

parser :: Parser [(Bool, Cuboid)]
parser = sepBy1 step endOfLine
  where
    step = (\on x' y' z' -> (on, Cuboid x' y' z')) <$> switchOn <* char ' ' <*> range 'x' <*> (char ',' *> range 'y') <*> (char ',' *> range 'z')
    switchOn = (== "on") <$> (try (string "on") <|> string "off")
    range dim = (,) <$> (char dim *> char '=' *> int) <*> (string ".." *> int)
    int = read <$> (many1 digit <|> string "-" <> many1 digit)
