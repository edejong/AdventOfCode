{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExplicitNamespaces #-}
module Day20.TrenchMap where

import qualified Data.Array.Repa as R
import Data.Array.Repa (Source (Array, extent), DIM2, U, DIM1, D, Z (Z),type  (:.) ((:.)), sumAllS, fromListUnboxed, ix1, computeS, (!), inShape, ix2)
import Data.List (unfoldr, foldl')
import Data.List.Split (chunksOf)
import Text.Parsec ( endOfLine, oneOf, sepBy1, many )
import Text.Parsec.String ( parseFromFile, Parser )

type Image r = Array r DIM2 Int
type EnhancementAlgo = Array U DIM1 Int

main :: IO ()
main = do
    (algo, im) <- either (error . show) id <$> parseFromFile parser "2021/data/day20.txt"
    let im' = enhanceN algo 50 im
    putStrLn $ showImg im'
    print $ sumAllS im'
  where nop src = R.traverse src id id

------ Parser

parser = (,) <$> imageEnhancementAlgo <* many endOfLine <*> image
imageEnhancementAlgo = toArr . map fromChar <$> many (oneOf ".#")
  where toArr xs = fromListUnboxed (ix1 (length xs)) xs
image = toImg <$> sepBy1 (many (oneOf ".#")) endOfLine
  where toImg xss = fromListUnboxed (ix2 (length xss) (length . head $ xss)) (map fromChar . concat $ xss)
fromChar c = if c == '.' then 0 else 1
toChar x = if x == 0 then '.' else '#'

------ Image manipulation

showImg :: Image U -> String
showImg im = unlines . chunksOf w . map toChar . R.toList $ im
  where (Z :. h :. w) = extent im

enhanceN :: EnhancementAlgo -> Int -> Image U -> Image U
enhanceN algo n src = fst . (!! n) . iterate (\(src', x:xs) -> (computeS $ enhance algo x src', xs)) $ (src, outOfBoundsValues)
  where outOfBoundsValues
         | algo ! ix1 0 == 0 = repeat 0
         | algo ! ix1 511 == 0 = cycle [0, 1]
         | otherwise = 0 : repeat 1

enhance :: (Source r Int) => EnhancementAlgo -> Int -> Image r -> Image D
enhance algo outOfBoundsValue src = R.traverse src resize draw
  where resize (Z :. h :. w) = Z :. (h+2) :. (w+2)
        draw lookupSrc (Z :. y :. x) =
            decode [if inShape (extent src) srcPos then lookupSrc srcPos else outOfBoundsValue
                   | dy <- [-2..0], dx <- [-2..0], let srcPos = Z :. y + dy :. x + dx]
        decode xs = let index = foldl' (\result x -> result * 2 + x) (head xs) (tail xs)
                    in algo ! ix1 index