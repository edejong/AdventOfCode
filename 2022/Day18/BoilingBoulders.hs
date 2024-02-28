{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
import           Control.Monad      (forM_, when)
import           Control.Monad.ST   (ST)
import           Data.Array.Base    ((!))
import           Data.Array.MArray
import           Data.Array.ST
import           Data.List.Split    (splitOn)

main :: IO ()
main = do
    xs <- map ((\[x,y,z] -> (x,y,z)) . map (read @Int) . splitOn ",") . lines <$> readFile "2022/Day18/day18.txt"
    let arr = runSTArray do
            arr' <- newArray ((-1,-1,-1), (20,20,20)) 0 :: ST s (Arr s)
            forM_ xs (\p -> writeArray arr' p 1)
            floodFill arr' (-1,-1,-1) 0 2
            return arr'
    print $ sum . map (length . filter (\p -> arr ! p /= 1) . neighbours) $ xs
    print $ sum . map (length . filter (\p -> arr ! p == 2) . neighbours) $ xs

type Point = (Int,Int,Int)
type Arr s = STArray s (Int, Int, Int) Int

neighbours :: Point -> [Point]
neighbours (x,y,z) = [(x-1,y,z), (x+1,y,z), (x,y-1,z), (x,y+1,z), (x,y,z-1), (x,y,z+1)]

floodFill :: Arr s -> Point -> Int -> Int -> ST s ()
floodFill arr p src dst = do
    bnds <- getBounds arr
    when (inRange bnds p) do
        v <- readArray arr p
        when (v == src) $ do
            writeArray arr p dst
            forM_ (neighbours p) (\p' -> floodFill arr p' src dst)
