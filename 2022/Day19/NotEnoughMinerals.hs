import           Control.Applicative (Applicative (liftA2), liftA3)
import           Control.Lens
import           Data.Foldable       (toList)
import           GHC.Base            (when)
import           Linear              (V4 (..), basis, transpose, (*^))

main :: IO ()
main = do
    bps <- map parseBlueprint . lines <$> readFile "2022/Day19/day19.txt"
    let maxG t b = maxGeodes b t (V4 1 0 0 0) (V4 0 0 0 0)
    print $ sum $ zipWith (*) [1..] $ map (maxG 24) bps
    print $ product $ map (maxG 32) (take 3 bps)
  where
    parseBlueprint = mkBlueprint . (\xs -> map ((read @Int) . (xs !!)) [6, 12, 18, 21, 27, 30]) . words
    mkBlueprint xs = V4 (V4 (head xs) 0 0 0) (V4 (xs !! 1) 0 0 0) (V4 (xs !! 2) (xs !! 3) 0 0) (V4 (xs !! 4) 0 (xs !! 5) 0)

type Blueprint = V4 (V4 Int)

maxGeodes :: Blueprint -> Int -> V4 Int -> V4 Int -> Int
maxGeodes bp = maxGeodes'
  where
    maxRobots = maximum <$> transpose bp
    maxGeodes' 0 _robots mats = mats^._4
    maxGeodes' t robots mats =
        let reject = liftA2 (>=) robots maxRobots & _4 .~ False
            xs = do
                (rject,rc,rVec) <- zip3 (toList reject) (toList bp) basis
                let dt = timeNeeded rc t robots mats
                when (rject || dt > t) []
                let (t', rs, ms) = wait dt t robots mats
                pure (t', rs + rVec, ms - rc)
            x = wait t t robots mats
         in maximum . map (\ (t', rs, ms) -> maxGeodes' t' rs ms) $ (x:xs)

timeNeeded :: V4 Int -> Int -> V4 Int -> V4 Int -> Int
timeNeeded rc t robots mats = maximum $ mask * liftA3 timeNeeded' rc robots mats
  where
    mask = signum <$> rc
    timeNeeded' _ 0 _ = t + 1  -- always more than the time we have left, so infeasible
    timeNeeded' c r m = max 1 (1 + (c-m+(r-1)) `div` r)

wait :: Int -> Int -> V4 Int -> V4 Int -> (Int, V4 Int, V4 Int)
wait dt t rs ms = (t - dt, rs, ms + (dt *^ rs))
