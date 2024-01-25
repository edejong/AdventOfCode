{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
import Data.Char (isDigit, ord, chr)
import Data.Range (Range (..), Bound (..), BoundType (..), rangesOverlap, (+=+), fromRanges)
import Data.List (sortOn, foldl', tails, intercalate)
import Data.Maybe (fromJust)
import GHC.Arr (Array(), accumArray, amap, bounds, (!))
import Data.Ord
import Data.Function (on)
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Set (Set)
import qualified Data.Set as Set

main :: IO ()
main = do
    xs <- lines <$> readFile "2023/Day22/day22.txt"

    let bs = zipWith mkBlock [1..] . map (map (read @Integer) . words . map (\c -> if isDigit c then c else ' ')) $ xs

    let (maxX, maxY, _) = getBounds bs
    let floorBlock = Block 0 (0 +=+ maxX) (0 +=+ maxY) (0 +=+ 0)

    let bs' = settleBlocks $ floorBlock : bs
    putStrLn $ showBlocks 'x' bs'
    putStrLn $ showBlocks 'y' bs'

    let edges = map (\(b:bs) -> (lbl b, map lbl $ findSupports b bs')) . filter (not . null) . tails $ bs'
    let parents = Map.fromList edges
    let children = Map.fromListWith (++) ([(k,[]) | (k,_) <- edges] ++ [(p,[c]) | (c,ps) <- edges, p <- ps])

    let canDisintegrate brick = all (\c -> length (parents Map.! c) > 1) $ children Map.! brick

    print $ length $ filter (canDisintegrate . lbl) bs
    print $ sum . map (subtract 1 . length . dropBrick (parents, children) . lbl) $ bs

dropBrick :: (Map Int [Int], Map Int [Int]) -> Int -> Set Int
dropBrick (ps, cs) brick = drop' Set.empty [brick]
  where
    drop' dropped [] = dropped
    drop' dropped (x:xs) = do
        let dropped' = Set.insert x dropped
        let allParentsHaveDropped b = all (`Set.member` dropped') (ps Map.! b)
        let fallingChildren = filter allParentsHaveDropped (cs Map.! x)
         in drop' dropped' (xs ++ fallingChildren)

data Brick = Block { lbl::Int, x::Range Integer, y::Range Integer, z::Range Integer } deriving (Eq)

instance Show Brick where
  show :: Brick -> String
  show b = show (lbl b) ++ " " ++
            show (getMin x b) ++ "," ++ show (getMin y b) ++ "," ++ show (getMin z b) ++ "~" ++
            show (getMax x b) ++ "," ++ show (getMax y b) ++ "," ++ show (getMax z b)

mkBlock :: Int -> [Integer] -> Brick
mkBlock label [x1, y1, z1, x2, y2, z2] = Block label (x1 +=+ x2) (y1 +=+ y2) (z1 +=+ z2)
mkBlock _ _ = undefined

getBounds :: [Brick] -> (Integer, Integer, Integer)
getBounds = foldl' (\(x1,y1,z1) (x2,y2,z2) -> (max x1 x2,max y1 y2,max z1 z2)) (0,0,0) . map (\b -> (getMax x b, getMax y b, getMax z b))

getMin :: (Brick -> Range Integer) -> Brick -> Integer
getMin f = fromJust . fst . rangeBnds . f

getMax :: (Brick -> Range Integer) -> Brick -> Integer
getMax f = fromJust . snd . rangeBnds . f

showBlocks :: Char -> [Brick] -> String
showBlocks 'x' bs =
    let (maxX, _maxY, maxZ) = getBounds bs
        bs' = bs --sortOn (getMin y) bs
        es = [((z',x'), (y', '.')) | x' <- [0..maxX], y' <- [10000], z' <- [0..maxZ] ]
        es' = map (\((x', y', z'), lbl') -> ((z',x'),(y',intToChar lbl'))) . concatMap blockCoords $ bs'
        f :: ((Integer, Char) -> (Integer, Char) -> (Integer, Char))
        f (10000,a) (y2,b) = (y2,b)
        f (y1,a) (y2,b) = let c = if a == b then a else '?' in if y1 < y2 then (y1,c) else (y2,c)
        arr = accumArray f (10000,'@') ((0,0),(maxZ,maxX)) (es ++ es')
     in unlines . reverse . showArray2D . amap snd $ arr

showBlocks 'y' bs =
    let (_maxX, maxY, maxZ) = getBounds bs
        bs' = bs --sortOn (getMin y) bs
        es = [((z',y'), (x', '.')) | x' <- [10000], y' <- [0..maxY], z' <- [0..maxZ] ]
        es' = map (\((x', y', z'), lbl') -> ((z',y'),(x',intToChar lbl'))) . concatMap blockCoords $ bs'
        f :: ((Integer, Char) -> (Integer, Char) -> (Integer, Char))
        f (10000,a) (x2,b) = (x2,b)
        f (x1,a) (x2,b) = let c = if a == b then a else '?' in if x1 < x2 then (x1,c) else (x2,c)
        arr = accumArray f (10000,'@') ((0,0),(maxZ,maxY)) (es ++ es')
     in unlines . reverse . showArray2D . amap snd $ arr

showBlocks _ _ = undefined

intToChar :: Int -> Char
intToChar c = chr (ord 'A' + c)

showArray2D :: Array (Integer, Integer) Char -> [String]
showArray2D arr =
    let (bndsLo, bndsHi) = bounds arr
        rs = [fst bndsLo..fst bndsHi]
        cs = [snd bndsLo..snd bndsHi]
     in [[arr ! (r,c) | c <- cs] | r <- rs]

blockCoords :: Brick -> [((Integer, Integer, Integer), Int)]
blockCoords b = [((x',y',z'), lbl b) | x' <- fromRanges [x b] , y' <- fromRanges [y b], z' <- fromRanges [z b]]

settleBlocks :: [Brick] -> [Brick]
settleBlocks bs =
    let bs' = sortOn (Down . getMin z) $ bs in settle bs'
  where
    settle :: [Brick] -> [Brick]
    settle [b] = [b]
    settle (b2:xs) = do
        let xs' = settle xs
        let b1 = head . sortOn (Down . getMax z) . filter (blocksOverlap b2) $ xs'
            floorZ = getMax z b1 + 1
            b2z = getMin z b2
            dz = b2z - floorZ
            b2z' = subtract dz <$> z b2
            b2' = b2 { z = b2z' }
        b2':xs'
    settle [] = []

blocksOverlap :: Brick -> Brick -> Bool
blocksOverlap a b = (rangesOverlap `on` x) a b && (rangesOverlap `on` y) a b

findSupports :: Brick -> [Brick] -> [Brick]
findSupports b = filter (blocksOverlap b) . filter (\bUnder -> getMin z b == 1 + getMax z bUnder)

------ Ranges

rangeBnds :: (Enum a) => Range a -> (Maybe a, Maybe a)
rangeBnds = \case
    (SingletonRange x) -> (Just x, Just x)
    (SpanRange x y) -> (lo x, hi y)
    (LowerBoundRange x) -> (lo x, Nothing)
    (UpperBoundRange y) -> (Nothing, hi y)
    InfiniteRange -> (Nothing, Nothing)
  where
    lo (Bound x t) = Just $ case t of { Exclusive -> succ x; _ -> x }
    hi (Bound x t) = Just $ case t of { Exclusive -> pred x; _ -> x }
