import GHC.Arr (listArray, amap, assocs, accumArray, bounds, Array, inRange, (!))
import Linear (V2(..), (*^))
import Data.Char (chr, ord)
import Control.Applicative (liftA2)
import Data.Bifunctor (Bifunctor (bimap))
import Data.List (nub, unfoldr)

main :: IO ()
main = do
    xss <- lines <$> readFile "2022/Day24/day24.txt"
    let (nRows,nCols) = (length xss, length . head $ xss)
        arr = listArray (V2 1 1,V2 nRows nCols) (concat xss)
        (start, goal) = (V2 1 2, snd (bounds arr) - V2 0 1)
        reverseRoute (src, dst, i) = Just (i, (dst, src, shortestPath arr src dst i))
    print $ take 4 $ unfoldr reverseRoute (start, goal, 0)

shortestPath :: Array (V2 Int) Char -> V2 Int -> V2 Int -> Int -> Int
shortestPath arr start goal t = bfs t [start]
  where
    (n, s, w, e) = (V2 (-1) 0, V2 1 0, V2 0 (-1), V2 0 1)
    nbrs = V2 0 0 : [n, s, w, e]
    bfs i ps
      | goal `elem` ps = i
      | otherwise =
        let blz = blizz arr (i+1)
            ps1 = [p+v | p <- ps, v <- nbrs]
            ps2 = filter ((=='.').(blz!)) . filter (inRange (bounds arr)) $ ps1
         in bfs (i + 1) (nub ps2)

blizz :: Array (V2 Int) Char -> Int -> Array (V2 Int) Char
blizz arr m =
    let wrap (lo,hi) p = liftA2 mod (p-lo) (hi-lo+1) + lo
        bnds = bimap (+1) (subtract 1) . bounds $ arr
        (n, s, w, e) = (V2 (-1) 0, V2 1 0, V2 0 (-1), V2 0 1)
        xs = map (\ (i,c) ->
          case c of
            '^' -> (wrap bnds $ i + m *^ n, c)
            'v' -> (wrap bnds $ i + m *^ s, c)
            '<' -> (wrap bnds $ i + m *^ w, c)
            '>' -> (wrap bnds $ i + m *^ e, c)
            _   -> (i, c)
            ) . filter ((/='.').snd) . assocs $ arr
        toChar = \case { [] -> '.'; [c] -> c; cs -> chr . (+ ord '0') . length $ cs }
        arr' = amap toChar . accumArray (flip (:)) [] (bounds arr) $ xs
    in arr'
