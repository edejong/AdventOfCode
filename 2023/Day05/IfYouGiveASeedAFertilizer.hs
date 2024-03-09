{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
import           Data.List            (foldl')
import           Data.List.Split      (chunksOf)
import           Data.Range
import           Text.Parsec
import           Text.Parsec.Language (haskellDef)
import           Text.Parsec.String   (Parser, parseFromFile)
import qualified Text.Parsec.Token    as P

main :: IO ()
main = do
    xs <- parseFromFile input "2023/Day05/day05.txt"
    let (seeds, maps) = either (error . show) id xs
    let f s = foldl' mapRanges s maps
    let seeds1 = joinRanges $ map SingletonRange seeds
    print $ head $ fromRanges . f $ seeds1
    let seeds2 = map (\[a,b] -> a +=* (a+b)) . chunksOf 2 $ seeds
    print $ head $ fromRanges . f $ seeds2

mapRanges :: [Range Integer] -> [(Range Integer, Integer)] -> [Range Integer]
mapRanges xs [] = xs
mapRanges xs ((r, d):ms) =
    let diff = difference xs [r]
        ints = intersection xs [r]
        ints' = map (fmap (+d)) ints
    in joinRanges $ ints' ++ mapRanges diff ms

input :: Parser ([Integer], [[(Range Integer, Integer)]])
input = (,) <$> seeds <*> count 7 categoryMap
  where
    seeds = string "seeds: " *> many1 (lexeme decimal)
    categoryMap = sepBy1 (P.identifier lexer) (char '-') <* lexeme (string "map:") >> many1 conversion
    conversion = (\[a, b, c] -> (b +=+ (b+c-1), a-b)) <$> count 3 (lexeme decimal)
    lexer = P.makeTokenParser haskellDef
    decimal = P.decimal lexer
    lexeme = P.lexeme lexer
