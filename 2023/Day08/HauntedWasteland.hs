import Data.Map ((!))
import qualified Data.Map as M

main :: IO ()
main = do
  xs <- lines <$> readFile "2023/Day08/day08.txt"
  let route = head xs
  let len = length route
  let tmp = map (\s -> (take 3 s, (take 3 . drop 7 $ s, take 3 . drop 12 $ s))) . drop 2 $ xs
  let nodes = M.fromList $ concatMap (\(a, (l, r)) -> [((a, i), let i' = ((i + 1) `mod` len) in if c == 'L' then (l, i') else (r, i')) | (i, c) <- zip [0 ..] route]) tmp
  let cycleLengths = map (followRoute nodes) (filter isStart (M.keys nodes))
  print @Integer $ foldr lcm (head cycleLengths) (tail cycleLengths)
  where
    isStart ([_, _, 'A'], 0) = True
    isStart _ = False
    followRoute _ ([_, _, 'Z'], _) = 0
    followRoute m pos = 1 + followRoute m (iterate (m !) pos !! 1)
