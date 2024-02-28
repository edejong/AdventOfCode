import           GHC.Arr (Array, array, (!))
import           Data.Tuple         (swap)

main :: IO ()
main = do
    xs <- map ((read @Integer) . drop 2 . dropWhile (/= ':')) . lines <$> readFile "2021/Day21/day21-test.txt"
    print $ uncurry (*) (playGame 1000 (head xs, 0) (xs !! 1, 0) 0)
    print $ uncurry max (countWins (head xs, 21) (xs !! 1, 21))

playGame :: Integer -> (Integer, Integer) -> (Integer, Integer) -> Integer -> (Integer, Integer)
playGame maxScore (_, score1) (_, score2) numRolls | score2 >= maxScore = (numRolls, score1)
playGame maxScore p1@(pos1, score1) p2@(pos2, score2) numRolls =
    playGame maxScore p2 (pos1', score1') (numRolls + 3)
    where
        rolls = sum . map ((+1) . (`mod` 100) . (+numRolls)) $ [0, 1, 2]
        pos1' = ((pos1 + rolls - 1) `mod` 10) + 1
        score1' = score1 + pos1'

-- Bottom up dynamic programming turns out to be about 1/3 faster than top down with
-- memoization using MemoTrie
countWins :: (Integer, Integer) -> (Integer, Integer) -> (Integer, Integer)
countWins p1@(pos1, score1) p2@(pos2, score2) = cache ! (p1, p2)
  where
    cache :: Array ((Integer, Integer),(Integer, Integer)) (Integer, Integer)
    cache = array (((1, 0), (1, 0)), ((10, 27), (10, 27)))
              [(((pos1, score1), (pos2, score2)), countWins' pos1 score1 pos2 score2)
              | score1 <- [0..27], score2 <- [0..27],  pos1 <- [1..10],  pos2 <- [1..10]]
      where rolls = [d1+d2+d3 | d1 <- [1..3], d2 <- [1..3], d3 <- [1..3]]
            plus (x1, y1) (x2, y2) = (x1+x2, y1+y2)
            countWins' pos1 score1 pos2 score2 = swap $ foldr (\x memo -> memo `plus` cached pos2 score2 (pos1' x) (score1' x)) (0, 0) rolls
              where cached _ _ _ score2 | score2 <= 0 = (0, 1)
                    cached pos1 score1 pos2 score2 = cache ! ((pos1, score1), (pos2, score2))
                    pos1' n = ((pos1 + n - 1) `mod` 10) + 1
                    score1' = (score1 -) . pos1'
