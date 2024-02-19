import           Control.Applicative (Applicative (liftA2))
import           Control.Lens
import           Data.Bifunctor      (first, second)
import           Data.Char           (isDigit)
import           Data.List           (foldl')
import           Data.List.Split     (splitOn)
import           Geometry
import           GHC.Arr             (Array, array, assocs, bounds, inRange,
                                      listArray, (!))
import           Linear              (M33, R2 (..), V2 (..), V3 (..), _m22,
                                      identity, (!*!), (!*))

main :: IO ()
main = do
    [xs, ys] <- splitOn "\n\n" <$> readFile "2022/Day22/day22.txt"
    let route = parseRoute ys
        xss = lines xs
        (nRows, nCols) = (length xss, maximum . map length $ xss)
        xss' = map (\ row -> let n = nCols - length row in row ++ replicate n ' ') xss
        arr = listArray (V2 1 1, V2 nRows nCols) (concat xss')

    let (up, down, left, right) = (V2 (-1) 0, V2 1 0, V2 0 (-1), V2 0 1)
        seams =
        --   [ -- (these are for day22-ex.txt)
        --     (V2 8 12, V2 5 12, right, V2 9 13, V2 9 16, up) -- cyan
        --   , (V2 12 12, V2 12 09, down, V2 8 1, V2 8 4, down) -- pink
        --   ]
          [
            (V2 1 51, V2 1 100, up, V2 151 1, V2 200 1, left)
          , (V2 200 50, V2 200 1, down, V2 1 150, V2 1 101, up)
          , (V2 100 51, V2 51 51, left, V2 101 50, V2 101 1, up)
          , (V2 50 51, V2 1 51, left, V2 101 1, V2 150 1, left)
          , (V2 200 50, V2 151 50, right, V2 150 100, V2 150 51, down)
          , (V2 50 101, V2 50 150, down, V2 51 100, V2 100 100, right)
          , (V2 50 150, V2 1 150, right, V2 101 100, V2 150 100, right)
          ]

    let ss = [stitch (a1', a2') (b1', b2')
             | (a1, a2, aDir, b1, b2, bDir) <- seams
             , (a1', a2', b1', b2') <- [(a1 + aDir, a2 + aDir, b1, b2), (b1 + bDir, b2 + bDir, a1, a2)]]

    let (arr1, pw1) = findPassw arr [] route
    let (arr2, pw2) = findPassw arr ss route
    putStrLn $ showArr arr1
    putStrLn $ showArr arr2
    print (pw1, pw2)

type Pos = V2 Int
type Att = (Pos, Dir)
type Arr = Array Pos Char
data Instruction = L | R | Move Integer deriving Show
type Dir = V2 Int
type Seam = ((Pos, Pos), M33 Int)

showArr :: Arr -> String
showArr arr = let (V2 r1 c1,V2 r2 c2) = bounds arr
               in unlines [[arr ! V2 r c | c <- [c1..c2]] | r <- [r1..r2]]

parseRoute :: String -> [Instruction]
parseRoute = \case
    []       -> []
    ('L':xs) -> L : parseRoute xs
    ('R':xs) -> R : parseRoute xs
    xs       -> let (ns,xs') = span isDigit xs in (Move . read $ ns) : parseRoute xs'

stitch :: (Pos, Pos) -> (Pos, Pos) -> Seam
stitch (a1, a2) (b1, b2) =
    let adir = signum <$> a2 - a1
        bdir = signum <$> b2 - b1
        rot = head $ filter (\r -> (r !* adir) == bdir) [identity, rot90, rot180, rot270]
        mtx = translate b1 !*! m22_to_m33 rot !*! translate (-a1)
        (l, h) = if a1 < a2 then (a1, a2) else (a2, a1)
     in ((l, h), mtx)

findPassw :: Arr -> [Seam] -> [Instruction] -> (Arr, Int)
findPassw arr seams route =
    let steps = followRoute (arr, seams) route (V2 1 1, V2 0 1)
        steps' = map (second dirToChar) steps
        arr2 = array (bounds arr) (assocs arr ++ steps')
        passw = password . last $ steps
        in (arr2, passw)
  where
    dirToChar = \case
        V2 0 1 -> '>'; V2 1 0 -> 'V'; V2 0 (-1) -> '<'; V2 (-1) 0 -> '^'; _ -> undefined

password :: Att -> Int
password (V2 r c, d) = 1000 * r + 4 * c + orientation d
  where
    orientation = \case
        V2   0    1  -> 0
        V2   1    0  -> 1
        V2   0  (-1) -> 2
        V2 (-1)   0  -> 3
        _ -> undefined

followRoute :: (Arr, [Seam]) -> [Instruction] -> Att -> [Att]
followRoute (arr, seams) = followRoute'
  where
    followRoute' [] _ = []
    followRoute' (i:is) att@(pos,dir) =
      case i of
        L        -> let att' = (pos, rot90  !* dir) in att' : followRoute' is att'
        R        -> let att' = (pos, rot270 !* dir) in att' : followRoute' is att'
        (Move 0) -> followRoute' is att
        (Move n) -> let att' = next (pos, dir)
                     in case arr ! fst att' of
                        '.' -> att' : followRoute' (Move (n-1):is) att'
                        '#' -> followRoute' is att
                        _   -> undefined
    next a = let f = first wrap . handleSeams . step in until ((/=' ').(arr!).fst) f (f a)
    step (p, d) = (p+d, d)
    wrap p = let (lo, hi) = bounds arr in liftA2 mod (p - lo) hi + lo
    handleSeams att'@(p,_) = foldl' (flip crossSeam) att' [mtx | (bnds, mtx) <- seams, inRange bnds p]
    crossSeam mtx (V2 x1 x2, d) = ((mtx !* V3 x1 x2 1)^._xy, mtx^._m22 !* d)
