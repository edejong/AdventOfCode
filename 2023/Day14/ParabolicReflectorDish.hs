import CycleDetect
import Data.List (transpose)

main :: IO ()
main = do
  xss <- lines <$> readFile "2023/Day14/day14.txt"
  print $ weigh $ transpose . map tilt . transpose $ xss
  let (mu, la) = findCycle cycleOnce xss
  let k = (1000000000 - mu) `div` la
  let x = fromInteger $ mu + (1000000000 - (mu + k * la))
  print $ weigh $ iterate cycleOnce xss !! x

tilt :: [Char] -> [Char]
tilt ('#' : xs) = '#' : tilt xs
tilt [] = []
tilt xs =
  let (as, bs) = span (/= '#') xs
      k = length . filter (== 'O') $ as
   in (replicate k 'O' ++ replicate (length as - k) '.') ++ tilt bs

weigh :: [[Char]] -> Integer
weigh = sum . map weigh' . transpose
  where
    weigh' = sum . map fst . filter ((== 'O') . snd) . zip [1 ..] . reverse

cycleOnce :: [[Char]] -> [[Char]]
cycleOnce xss =
  let xssN = transpose . map tilt . transpose $ xss
      xssW = map tilt xssN
      xssS = reverse . transpose . map tilt . transpose . reverse $ xssW
      xssE = map ((reverse . tilt) . reverse) xssS in xssE
