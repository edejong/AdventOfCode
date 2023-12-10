{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
module Day07.CamelCards where
import Data.List (elemIndex, sortOn)
import Data.Maybe (fromJust)
import qualified Data.Map as M ( fromListWith, toList )
import Data.Ord

main :: IO ()
main = do
    xs <- map words . lines <$> readFile "2023/data/day07-test.txt"
    let xs1 = map (\[a,b] -> (mkHand "23456789TJQKA" a, read @Int b)) xs
    print $ sum . zipWith (*) [1..] . map snd . sortOn fst $ xs1

    let xs2 = map (\[a,b] -> (mkHand "J23456789TQKA" a, read @Int b)) xs
    print $ sum . zipWith (*) [1..] . map snd . sortOn fst $ xs2

card :: [Char] -> Char -> Int
card xs = (+2) . fromJust . flip elemIndex xs

data Hand = Hand [Int] deriving (Eq, Show)

mkHand :: [Char] -> [Char] -> Hand
mkHand xs = Hand . map (card xs)

instance Ord Hand where
  compare (Hand xs) (Hand ys) = compare (score xs : xs) (score ys : ys)

hist = sortOn (Down . snd) . M.toList . M.fromListWith (+) . map (, 1)

score xs = 
    let xs' = hist xs
        jokers = length . filter (==1) $ xs
    in
    case map snd xs' of
        [5] -> 6       -- J: 6
        [4,1] -> 5     -- J: 6
        [3,2] -> 4     -- J: 6
        [3,1,1] -> 3   -- J: 5
        [2,2,1] -> 2   -- J: 4 or 5
        [2,1,1,1] -> 1 -- J: 3
        _ -> 0
