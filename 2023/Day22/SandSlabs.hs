import           Control.Lens hiding (children)
import           Data.Char    (isDigit)
import           Data.IntMap  (IntMap, (!))
import qualified Data.IntMap  as IntMap
import           Data.List    (foldl', sortOn)
import           Data.Ord     (Down (Down))
import           Data.Set     (Set)
import qualified Data.Set     as Set
import           Linear

-- Fairly brute force solution, not using any intelligent sorting when searching.
-- Yet surprisingly fast.

main :: IO ()
main = do
    xs <- lines <$> readFile "2023/Day22/day22.txt"

    let bs = zipWith mkBrick [1..] . map (map read . words . map (\c -> if isDigit c then c else ' ')) $ xs

    let (V3 maxX maxY _) = getBounds' bs
    let floorBlock = Brick 0 (V2 (V3 0 0 0) (V3 maxX maxY 0))

    let bs' = settleBlocks $ floorBlock : bs

    let edges = map (\b -> (lbl b, map lbl $ findSupports b bs')) bs'
    let parents = IntMap.fromList edges
    let children = IntMap.fromListWith (++) ([(k,[]) | (k,_) <- edges] ++ [(p,[c]) | (c,ps) <- edges, p <- ps])

    let canDisintegrate brick = all (\c -> length (parents ! c) > 1) $ children ! brick

    print $ length $ filter (canDisintegrate . lbl) bs
    print $ sum . map (subtract 1 . length . dropBrick (parents, children) . lbl) $ bs
  where
    findSupports b = filter (blocksOverlap b) . filter (\bUnder -> minZ b == 1 + maxZ bUnder)

data Brick = Brick { lbl::Int, getBounds :: V2 (V3 Int) } deriving (Eq)

mkBrick :: Int -> [Int] -> Brick
mkBrick label [x1, y1, z1, x2, y2, z2] = Brick label (V2 (V3 x1 y1 z1) (V3 x2 y2 z2))
mkBrick _ _ = undefined

dropBrick :: (IntMap [Int], IntMap [Int]) -> Int -> Set Int
dropBrick (ps, cs) brick = drop' Set.empty [brick]
  where
    drop' dropped [] = dropped
    drop' dropped (x:xs) = do
        let dropped' = Set.insert x dropped
        let allParentsHaveDropped b = all (`Set.member` dropped') (ps ! b)
        let fallingChildren = filter allParentsHaveDropped (cs ! x)
         in drop' dropped' (xs ++ fallingChildren)

getBounds' :: [Brick] -> V3 Int
getBounds' = foldl' (liftU2 max) (V3 0 0 0) . map ((^._2) . getBounds)

minZ :: Brick -> Int
minZ b = getBounds b ^. (_1 . _z)
maxZ :: Brick -> Int
maxZ b = getBounds b ^. (_2 . _z)

settleBlocks :: [Brick] -> [Brick]
settleBlocks bs =
    let bs' = sortOn (Down . minZ) bs in settle bs'
  where
    settle :: [Brick] -> [Brick]
    settle [b] = [b]
    settle (b2:xs) = do
        let xs' = settle xs
            b1 = head . sortOn (Down . maxZ) . filter (blocksOverlap b2) $ xs'
            dz = minZ b2 - (maxZ b1 + 1)
            b2bnds' = getBounds b2 & (_x._z) -~ dz & (_y._z) -~ dz
            b2' = b2 { getBounds = b2bnds' }
        b2':xs'
    settle [] = []

blocksOverlap :: Brick -> Brick -> Bool
blocksOverlap a b =
    let ((ax, ay), (bx, by)) = (getXYRange . getBounds $ a, getXYRange . getBounds $ b)
     in rangesOverlap ax bx && rangesOverlap ay by
  where
    getXYRange (V2 lo hi) = ((lo ^._x, hi ^._x), (lo ^._y, hi ^._y))
    rangesOverlap (x1,x2) (y1,y2) = x1 <= y2 && y1 <= x2
