{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
import           Control.Lens
import           Data.Char
import           Data.List       (transpose)
import           Data.List.Split (splitOn)

main :: IO ()
main = do
    [xs, ys] <- splitOn "\n\n" <$> readFile "2022/Day05/day05.txt"
    let xs' = map (dropWhile isSpace) . transpose . map getChars . init . lines $ xs
    let ys' = map parseMoveLine . lines $ ys
    print (getResult reverse xs' ys', getResult id xs' ys')
  where
    getResult f xs = map head . move f xs
    getChars xs = [c | (i, c) <- zip (cycle [(0::Int)..3]) xs, i == 1]
    parseMoveLine = map (read @Int) . words . filter (\c -> isDigit c || isSpace c)
    move _ stacks [] = stacks
    move f stacks ([count, fromIndex, toIndex]:xs) =
        let fromStack = stacks !! (fromIndex - 1)
            toStack = stacks !! (toIndex - 1)
            stacks' = stacks & ix (fromIndex - 1) .~ drop count fromStack
                            & ix (toIndex - 1) .~ ((f . take count) fromStack <> toStack)
        in move f stacks' xs
