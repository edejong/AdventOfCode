{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
import           Data.List.Split (splitOn)
import           Data.Map        (Map, insertWith, keys, singleton, unionWith,
                                  unionsWith, (!))
import qualified Data.Map        as Map

main :: IO ()
main = do
    ls <- lines <$> readFile "2021/Day14/day14-test.txt"
    let template = head ls
    let rules = foldr ((\[k,v] m -> Map.insert k (head v) m) . splitOn " -> ") Map.empty . drop 2 $ ls

    let x0 = Map.fromList . map (\k -> (k, hist k)) $ keys rules
    let xs = iterate (step rules) x0

    let tmp = unionsWith (+) . map ((xs !! 40) !) $ zipWith (\a b -> [a, b]) template (tail template)
    let tmp' = unionWith (+) tmp (Map.fromList . (map (,-1) . init . tail) $ template)

    print $ maximum tmp' - minimum tmp' - 1 -- Why do I need to subtract 1??

hist :: [Char] -> Map Char Int
hist = foldr (\x m -> insertWith (+) x 1 m) Map.empty

step :: Map String Char -> Map String (Map Char Int) -> Map String (Map Char Int)
step rules m = Map.mapWithKey (\k _ -> step' k (rules ! k)) m
  where
    step' [a,c] b = Map.unionsWith (+) [m ! [a,b], m ! [b,c], singleton b (-1)]
