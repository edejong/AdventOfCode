{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
module Day07.CamelCards where
import Data.List (elemIndex, sortOn, nub)
import Data.Maybe (fromJust)
import qualified Data.Map as M ( fromListWith, toList )
import Data.Ord

main :: IO ()
main = do
    xs <- map words . lines <$> readFile "2023/data/day07.txt"
    let xs1 = map (\[a,b] -> (mkHand "23456789TJQKA" a, read @Int b)) xs
    print $ sum . zipWith (*) [1..] . map snd . sortOn (\x -> let xs = fst x in score xs : xs)  $ xs1

    let xs2 = map (\[a,b] -> (mkHand "J23456789TQKA" a, read @Int b)) xs
    print $ sum . zipWith (*) [1..] . map snd . sortOn (\x -> let xs = fst x in score' xs : xs) $ xs2
    

mkHand xs = map (fromJust . flip elemIndex xs)

hist = sortOn (Down . snd) . M.toList . M.fromListWith (+) . map (, 1)

score xs = 
    case map snd (hist xs) of
        [5] -> 6       -- J: 6
        [4,1] -> 5     -- J: 6
        [3,2] -> 4     -- J: 6
        [3,1,1] -> 3   -- J: 5
        [2,2,1] -> 2   -- J: 4 or 5
        [2,1,1,1] -> 1 -- J: 3
        _ -> 0

score' xs = maximum $ map score $ nub $ xs:[map (\k -> if k == 0 then c else k) xs | c <- nub xs, c /= 0]
