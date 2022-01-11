module Day19.BeaconScanner where

import Numeric.LinearAlgebra
import qualified Numeric.LinearAlgebra as LA
import Text.Parsec
import Text.Parsec.String ( parseFromFile )
import Data.List ( nub, sort )

type Scanner = [Vector R]

main :: IO ()
main = do
    Right xs <- parseFromFile parser "2021/data/day19.txt"
    let (bs,ps) = combineAllScanners xs
    print $ length bs
    print $ maximum [manhattanDist p1 p2 | p1 <- ps, p2 <- ps]

manhattanDist :: Vector R -> Vector R -> R
manhattanDist p1 p2 = sum . map abs . toList $ (p1 - p2)

magSq :: Numeric t => Vector t -> t
magSq v = v <.> v

dists :: Scanner -> [[R]]
dists scanner = [sort ([magSq (b2 - b1) | b2 <- scanner]) | b1 <- scanner]

probablySameBeacon :: [R] -> [R] -> Bool
probablySameBeacon xs ys = probablySameBeacon' xs ys 0
  where
    probablySameBeacon' _ _ 12 = True
    probablySameBeacon' (x:xs) (y:ys) n
      | x < y = probablySameBeacon' xs (y:ys) n
      | x > y = probablySameBeacon' (x:xs) ys n
      | otherwise = probablySameBeacon' xs ys (n+1)
    probablySameBeacon' _ _ _ = False

overlappingBeacons :: Scanner -> Scanner -> [(Vector R, Vector R)]
overlappingBeacons s1 s2 =
  [(b1, b2) | (b1, d1) <- zip s1 (dists s1),
              (b2, d2) <- zip s2 (dists s2), probablySameBeacon d1 d2]

affineMatrix :: [Vector R] -> [Vector R] -> Matrix R
affineMatrix a b =
  cmap (fromIntegral . round) . linearSolveLS (fromRows a ||| 1) $ (fromRows b ||| 1) :: Matrix R

combineAllScanners :: [Scanner] -> (Scanner, [Vector R])
combineAllScanners ss = combineAllScanners' (head ss) (tail ss) [] []

combineAllScanners' :: Scanner -> [Scanner] -> [Scanner] -> [Vector R] -> (Scanner, [Vector R])
combineAllScanners' s1 [] [] ps = (s1, ps)
combineAllScanners' s1 [] ss' ps = combineAllScanners' s1 ss' [] ps
combineAllScanners' s1 (s2:ss) ss' ps = case combineScanners s1 s2 of
  Just (s1',p) -> combineAllScanners' s1' ss ss' (p:ps)
  Nothing -> combineAllScanners' s1 ss (s2:ss') ps

combineScanners :: Scanner -> Scanner -> Maybe (Scanner, Vector R)
combineScanners a b
  | null overlapping = Nothing
  | otherwise = Just (a', p)
  where
    overlapping = overlappingBeacons a b
    m = uncurry (flip affineMatrix) . unzip $ overlapping
    b' = ((fromRows b ||| 1) LA.<> m) ?? (All, Take 3)
    a' = nub (a ++ toRows b')
    p =  head . toRows $ (row [0, 0, 0, 1] LA.<> m) ?? (All, Take 3)

------- Parser
parser = sepBy1 scanner endOfLine
scanner = string "--- scanner " *> many digit *> string " ---" *> endOfLine *> sepEndBy point endOfLine
point = (\x y z -> fromList [x, y, z]) <$> int <*> (char ',' *> int) <*> (char ',' *> int)
int = read <$> (many1 digit <|> string "-" Prelude.<> many1 digit)
