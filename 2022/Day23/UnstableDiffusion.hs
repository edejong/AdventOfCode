{-# OPTIONS_GHC -Wno-incomplete-uni-patterns
                -Wno-incomplete-patterns #-}
import GHC.Arr (accumArray, listArray, Array, bounds, assocs, (!), inRange, elems)
import Linear hiding (rotate, trace)
import Control.Applicative (liftA2)
import Data.List (foldl', find)

main :: IO ()
main = do
    xss <- lines <$> readFile "2022/Day23/day23.txt"
    let (nRows, nCols) = (length xss, length . head $ xss)
    let arr = listArray (V2 1 1, V2 nRows nCols) (concat xss)
    let rs = rounds arr
    let arr' = rs !! 10
    print $ length . filter (=='.') . elems $ arr'
    print $ length rs

type Pos = V2 Int

rounds :: Array Pos Char -> [Array Pos Char]
rounds = rounds' 0
  where
    rounds' i arr =
        let moves = proposeMoves arr i
            hasMoves = any (\ (p,ps) -> length ps == 1 && ps /= [p] ) . assocs $ moves
            next = if hasMoves then rounds' ((i+1) `mod` 4) (move moves) else []
            in arr : next

proposeMoves :: Array Pos Char -> Int -> Array Pos [Pos]
proposeMoves arr i =
    let xs = map ((\ p -> (proposeMove arr p i, p)) . fst) . filter ((=='#').snd) . assocs $ arr
        bnds = calcBounds $ map fst xs ++ map snd xs
        arr' = accumArray (flip (:)) [] bnds xs
     in arr'
  where
    calcBounds (p:ps) = (foldl' (liftA2 min) p ps, foldl' (liftA2 max) p ps)

move :: Array Pos [Pos] -> Array Pos Char
move arr =
    let xs = do
            (p, ps) <- filter (not.null.snd) . assocs $ arr
            case ps of { [_] -> [p]; _ -> ps }
     in (accumArray (const . const $ '#') '.' (bounds arr) . map (,'#')) xs

proposeMove :: Array Pos Char -> Pos -> Int -> Pos
proposeMove arr p i =
    let x = (map (p+) [nw, n, ne, w, e, sw, s, se], p)
        xs = rotate i [ (map (p+) [nw, n, ne], p + n)
                      , (map (p+) [sw, s, se], p + s)
                      , (map (p+) [nw, w, sw], p + w)
                      , (map (p+) [ne, e, se], p + e)]
    in maybe p snd . find (canMove . fst) $ x : xs
  where
    [nw, n, ne, w, _, e, sw, s, se] = [V2 r c | r <- [-1..1], c <- [-1..1]]
    canMove = not . any ((=='#').(arr!)) . filter (inRange (bounds arr))

rotate :: Int -> [a] -> [a]
rotate n xs = let k = length xs in take k . drop (n `mod` k) . cycle $ xs
