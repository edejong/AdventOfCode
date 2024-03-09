{-# LANGUAGE ExplicitNamespaces #-}
import           Data.Array.Repa    (D, DIM1, DIM2, Source (Array, extent), U,
                                     Z (Z), computeP, fromListUnboxed, inShape,
                                     ix1, ix2, sumAllS, type (:.) ((:.)), (!))
import qualified Data.Array.Repa    as R hiding (map)
import           Data.List          (foldl')
import           Data.List.Split    (chunksOf)
import           Text.Parsec        (endOfLine, many, oneOf, sepBy1)
import           Text.Parsec.String (parseFromFile)

type Image r = Array r DIM2 Int
type EnhancementAlgo = Array U DIM1 Int

main :: IO ()
main = do
    (algo, im) <- either (error . show) id <$> parseFromFile parser "2021/Day20/day20-test.txt"
    im1 <- enhanceN algo 2 im
    im2 <- enhanceN algo 50 im
    -- putStrLn $ showImg im1
    print $ sumAllS im1
    -- putStrLn $ showImg im2
    print $ sumAllS im2
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

enhanceN :: (Monad m) => EnhancementAlgo -> Int -> Image U -> m (Image U)
enhanceN algo n src = enhanceN' algo n src outOfBoundsValues
  where outOfBoundsValues
         | algo ! ix1 0 == 0 = repeat 0
         | algo ! ix1 511 == 0 = cycle [0, 1]
         | otherwise = 0 : repeat 1
        enhanceN' _ 0 src _ = return src
        enhanceN' algo n src (x:xs) = do
            src' <- computeP $ enhance algo x src
            enhanceN' algo (n-1) src' xs
        enhanceN' _ _ _ _ = undefined

enhance :: (Source r Int) => EnhancementAlgo -> Int -> Image r -> Image D
enhance algo outOfBoundsValue src = R.traverse src resize draw
  where resize (Z :. h :. w) = Z :. (h+2) :. (w+2)
        draw lookupSrc (Z :. y :. x) =
            decode [if inShape (extent src) srcPos then lookupSrc srcPos else outOfBoundsValue
                   | dy <- [-2..0], dx <- [-2..0], let srcPos = Z :. y + dy :. x + dx]
        decode xs = let index = foldl' (\result x -> result * 2 + x) (head xs) (tail xs)
                    in algo ! ix1 index
