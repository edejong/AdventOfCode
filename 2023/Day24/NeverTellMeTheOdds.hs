{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
import           Control.Lens
import           Data.List    (intercalate, tails)
import           Data.Ratio   ((%))
import           Linear       (R2 (_xy), V2 (V2), V3 (V3))
-- import Math.MFSolve

main :: IO ()
main = do
    let parseHailstone = (\[px,py,pz,vx,vy,vz] -> mkHailstone px py pz vx vy vz) . map read . words . filter (`notElem` "@,")
    hs <- map parseHailstone . lines <$> readFile "2023/Day24/day24-ex.txt"
    let bnds = V2 7 27
    -- let bnds = V2 200000000000000 400000000000000
    let res = [(h1, h2, intersectInArea bnds (line2D h1) (line2D h2)) | (h1:hs') <- tails hs, h2 <- hs']
    -- putStrLn $ unlines . map (\(a,b,c) -> showTest a b c) $ res
    print $ length $ filter (\(_,_,r) -> (\case { (Inside _) -> True; _ -> False }) r) res

    -- PART 2
    -- Let (p, v) be the position and velocity of the rock, and (Pi, Vi),
    --   be those of hailstone i. Then
    --     p + ti * v == Pi + ti * Vi  <=>
    --     p - Pi == ti * (v - Vi)
    -- So (p - Pi) and (v - Vi) are parallel vectors, which means that
    --     (p - Pi) × (v - Vi) == 0
    -- Solving this set for three hailstones yields 9 equations. I cheated and
    -- used sympy for this as mfsolve does not seem to be working.

    print (669042940632377 :: Integer)

    -- let [p1, p2, p3, v1, v2, v3] = map (makeVariable . SimpleVar) . words $ "p1 p2 p3 v1 v2 v3" :: [Expr SimpleVar Double]

    -- showVars $ flip execSolver noDeps $ do
    --   (8 - v2)*(p3 - 134367602892386) + (p2 - 321166281702430)*(v3 - 338) === 0
    --   (338 - v3)*(p1 - 176253337504656) + (p3 - 134367602892386)*(v1 - 190) === 0
    --   (190 - v1)*(p2 - 321166281702430) + (p1 - 176253337504656)*(v2 - 8) === 0

    --   (303 - v2)*(p3 - 73640306314241) + (p2 - 112919194224200)*(v3 - 398) === 0
    --   (398 - v3)*(p1 - 230532038994496) + (p3 - 73640306314241)*(v1 - 98) === 0
    --   (98 - v1)*(p2 - 112919194224200) + (p1 - 230532038994496)*(v2 - 303) === 0

    --   (p2 - 321507930209081)*(v3 + 75) + (p3 - 325769499763335)*(-v2 - 119) === 0
    --   (p1 - 326610633825237)*(-v3 - 75) + (p3 - 325769499763335)*(v1 + 67) === 0
    --   (p1 - 326610633825237)*(v2 + 119) + (p2 - 321507930209081)*(-v1 - 67) === 0

    --   (40 - v2)*(p3 - 222921323057425) + (p2 - 294541805320942)*(v3 + 40) === 0
    --   (p1 - 247597416289076)*(-v3 - 40) + (p3 - 222921323057425)*(v1 + 76) === 0
    --   (p1 - 247597416289076)*(v2 - 40) + (p2 - 294541805320942)*(-v1 - 76) === 0

    --   (p2 - 361186328321686)*(v3 + 65) + (p3 - 241687357180579)*(-v2 - 211) === 0
    --   (p1 - 219025967354328)*(-v3 - 65) + (p3 - 241687357180579)*(v1 - 58) === 0
    --   (58 - v1)*(p2 - 361186328321686) + (p1 - 219025967354328)*(v2 + 211) === 0

  -- where
  --   showTest h1 h2 result = unlines [
  --       "Hailstone A: " ++ show h1,
  --       "Hailstone B: " ++ show h2,
  --       show result]

data Hailstone = HS { pos::V3 Integer, vel::V3 Integer }

mkHailstone :: Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Hailstone
mkHailstone x0 y0 z0 dx dy dz = HS (V3 x0 y0 z0) (V3 dx dy dz)

instance Show Hailstone where
    show (HS (V3 x0 y0 z0) (V3 dx dy dz)) =
        let showV3 = (intercalate ", " . map show)
         in showV3 [x0, y0, z0] ++ " @ " ++ showV3 [dx, dy, dz]

type Line2D = (V2 Integer, V2 Integer)

line2D :: Hailstone -> Line2D
line2D hs =
    let point2D p = {-fromInteger <$>-} p ^._xy
     in (point2D $ pos hs, point2D (pos hs + vel hs))

data IntersectResult = Inside Intersection
                     | Outside Intersection
                     | InPast Intersection
                     | Parallel

type Intersection = (V2 Rational, Rational, Rational)

instance Show IntersectResult where
  show = \case
    (Inside (V2 x y, t, u)) -> "Hailstones' paths will cross inside the test area (at x=" ++ fmtNum x ++ ", y=" ++ fmtNum y ++ ", (t,u)=" ++ show ((fromRational t, fromRational u) :: (Double, Double)) ++  ")."
    (Outside (V2 x y, t, u)) -> "Hailstones' paths will cross outside the test area (at x=" ++ fmtNum x ++ ", y=" ++ fmtNum y ++ ", (t,u)=" ++ show ((fromRational t, fromRational u) :: (Double, Double)) ++  ")."
    (InPast (V2 x y, t, u)) -> "Hailstones' paths crossed in the past for " ++
        (if t < 0 && u < 0 then "both hailstones." else (if t < 0 then "hailstone A." else "hailstone B.")) ++ " (at x=" ++ fmtNum x ++ ", y=" ++ fmtNum y ++ ", (t,u)=" ++ show ((fromRational t, fromRational u) :: (Double, Double)) ++  ")."
    Parallel -> "Hailstones' paths are parallel; they never intersect."
   where
    fmtNum :: Rational -> String
    fmtNum = (\x -> show (x `div` 1000) ++ "." ++ show (x `mod` 1000)) . (floor :: Rational -> Integer) . (* 1000)

intersectInArea :: V2 Integer -> Line2D -> Line2D -> IntersectResult
intersectInArea area l1 l2 =
    case intersectLines2D l1 l2 of
        Just (p, t, u)
            | t < 0 || u < 0 -> InPast (p, t, u)
            | (not . inArea) p -> Outside (p, t, u)
            | otherwise -> Inside (p, t, u)
        _ -> Parallel
  where
    (V2 lo hi) = toRational <$> area
    inArea (V2 x y) = x >= lo && x <= hi && y >= lo && y <= hi

intersectLines2D :: Line2D -> Line2D -> Maybe Intersection
intersectLines2D (V2 x1 y1, V2 x2 y2) (V2 x3 y3, V2 x4 y4)
  | denom == 0 = Nothing
  | otherwise = Just (V2 (pxTimesD % denom) (pyTimesD % denom), t, u)
  where
    denom = (x1-x2)*(y3-y4) - (y1-y2)*(x3-x4)
    pxTimesD = (x1*y2 - y1*x2) * (x3-x4) - (x1-x2) * (x3*y4 - y3*x4)
    pyTimesD = (x1*y2 - y1*x2) * (y3-y4) - (y1-y2) * (x3*y4 - y3*x4)
    t = (pxTimesD-(denom*x1)) % (denom*(x2-x1))
    u = (pxTimesD-(denom*x3)) % (denom*(x4-x3))
