import           GHC.Arr (Array, assocs, bounds, listArray, (!), (//))
import           Linear  (V2 (..))

main :: IO ()
main = do
    xss <- lines <$> readFile "2021/Day25/day25.txt"
    let (nRows, nCols) = (length xss, length . head $ xss)
    let arr = listArray (V2 0 0,V2 (nRows-1) (nCols-1)) $ concat xss
    print $ length . step $ arr

step :: Array (V2 Int) Char -> [Array (V2 Int) Char]
step arr = do
    let (V2 rHi cHi) = snd . bounds $ arr
        east (V2 r c)  = V2 r ((c+1) `mod` (cHi+1))
        south (V2 r c) = V2 ((r+1) `mod` (rHi+1)) c
        move c f a = [ e
                     | (pos, c') <- assocs arr
                     , c == c'
                     , let pos' = f pos
                     , a ! pos' == '.'
                     , e <- [(pos, '.'), (pos', c)]
                     ]
        (xs, arr2) = (move '>' east arr, arr // xs)
        (ys, arr3) = (move 'v' south arr2, arr2 // ys)
     in if not (null xs && null ys) then arr : step arr3 else [arr]
