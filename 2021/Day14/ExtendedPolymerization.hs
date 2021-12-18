{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Day14.ExtendedPolymerization where
import Data.List.Split (splitOn)
import Data.Map (Map, (!), keys, insertWith, singleton, unionsWith, unionWith)
import qualified Data.Map as Map

main :: IO ()
main = do
    ls <- lines <$> readFile "2021/data/day14-test.txt"
    let template = head ls
    let rules = foldr ((\[k,v] m -> Map.insert k (head v) m) . splitOn " -> ") Map.empty . drop 2 $ ls

    print $ rules

    let x0 = Map.fromList . map (\k -> (k, hist k)) $ keys rules
    let xs = iterate (step rules) x0

    let tmp = unionsWith (+) . map ((xs !! 40) !) $ zipWith (\a b -> [a, b]) template (tail template)
    let tmp' = unionWith (+) tmp (Map.fromList . (map (,-1) . init . tail) $ template)

    print $ maximum tmp' - minimum tmp' - 1 -- Why do I need to subtract 1??

hist :: [Char] -> Map Char Int
hist = foldr (\x m -> insertWith (+) x 1 m) Map.empty

step :: Map String Char -> Map String (Map Char Int) -> Map String (Map Char Int)
step rules m = Map.mapWithKey (\k v -> step' k (rules ! k)) m
  where
    step' [a,c] b = Map.unionsWith (+) [m ! [a,b], m ! [b,c], singleton b (-1)]


-- Conclusion: Need to iterate on both pairs and single chars!

-- NNCB -> {NN: 1, NC: 1, CB: 1}, {N: 2, C: 1, B:1}
----------
-- (NCNBCHB)
-- NCN -> {NC: 1, CN: 1}, {N: 2, C: 1}
-- NBC -> {NB: 1, BC: 1}, {N: 1, B: 1, C: 1}
-- CHB -> {CH: 1, HB: 1}, {C: 1, H: 1, B: 1}
--------------------------------------------
-- {N:2, C:1, B:1} + C + B + H = {B:2, C:2, H:1, N:2}
-- {BC: 1, CH: 1,  ...


-- ("BB",'N'),("BC",'B'),("BH",'H'),("BN",'B'),
-- ("CB",'H'),("CC",'N'),("CH",'B'),("CN",'C'),
-- ("HB",'C'),("HC",'B'),("HH",'N'),("HN",'C'),
-- ("NB",'B'),("NC",'B'),("NH",'C'),("NN",'C')

-- NNCB -> [NN]: 1, N[C]: 1, C[B]: 1
-- NN -> NCN -> NC: 1, CN: 1
-- NC -> NBC -> NB: 1, BC: 1
-- CB -> CHB -> 

-- NNCB -> NN:1, NC:1, CB:1
-- BC:1, CH:1, CN:1, NB:1, NC:1, CH:1, HB:1
-- 