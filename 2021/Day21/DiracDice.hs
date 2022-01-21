{-# LANGUAGE TypeApplications #-}
module Day21.DiracDice where
import Control.Monad.State
    ( get, put, unless, runState, State, execState )
import Prelude hiding (turn)
import Data.Bifunctor (first, second, bimap)
import Debug.Trace (trace)
import Data.Char (isDigit)
import Data.Tuple (swap)
import Data.MemoTrie
import Data.Array.Unboxed (array)

main :: IO ()
main = do
    xs <- map ((read @Integer) . drop 2 . dropWhile (/= ':')) . lines <$> readFile "2021/data/day21.txt"
    print $ uncurry (*) (playGame 1000 (head xs, 0) (xs !! 1, 0) 0)
    print $ uncurry max (countWins (head xs, 21) (xs !! 1, 21))

playGame :: Integer -> (Integer, Integer) -> (Integer, Integer) -> Integer -> (Integer, Integer)
playGame maxScore (_, score1) (_, score2) numRolls | score2 >= maxScore = (numRolls, score1)
playGame maxScore p1@(pos1, score1) p2@(pos2, score2) numRolls =
    playGame maxScore p2 (pos1', score1') (numRolls + 3)
    where
        rolls = sum . map ((+1) . (`mod` 100) . (+numRolls)) $ [0, 1, 2]
        pos1' = move pos1 rolls
        score1' = score1 + pos1'

move :: Integer -> Integer -> Integer
move pos n = ((pos + n - 1) `mod` 10) + 1

countWins :: (Integer, Integer) -> (Integer, Integer) -> (Integer, Integer)
countWins = memo2 countWins'
  where
    countWins' :: (Integer, Integer) -> (Integer, Integer) -> (Integer, Integer)
    countWins' _ (_, score2) | score2 <= 0 = (0, 1)
    countWins' p1@(pos1, score1) p2@(pos2, score2) =
        swap $ foldr (\x memo -> memo `plus` countWins p2 (pos1' x, score1' x)) (0, 0) rolls
        where
            rolls = [d1+d2+d3 | d1 <- [1..3], d2 <- [1..3], d3 <- [1..3]]
            pos1' = move pos1
            score1' = (score1 -) . pos1'
            plus (x1, y1) (x2, y2) = (x1+x2, y1+y2)