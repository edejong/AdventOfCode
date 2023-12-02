{-# LANGUAGE TypeApplications #-}
module Day16.ProboscideaVolcanium where
import qualified Data.Map as Map
import Data.Map (Map, (!))
import Control.Monad.State
    ( modify, MonadState(get), State, evalState )
import Data.List.Split ( splitOn )
import Data.Char ( isSpace )
import Data.List (foldl', inits, tails, sort)
import Data.Bits ( Bits(clearBit, testBit) )

-- TODO: Cache is probably a lot faster with a mutable Array.
-- Alternatively, could try bottom-up DP.

main :: IO ()
main = do
    valves <- map parseValve . lines <$> readFile "2022/data/day16.txt"
    let valvesMap = simplify . Map.fromList . map (\v -> (label v, v)) $ valves
    let valveLabels = Map.fromList . flip zip [(0::Int)..] . reverse . sort . Map.keys $ valvesMap
    let valvesMap' = Map.fromList . map ((\v -> (label v, v)) . transformLabels (valveLabels !) . snd) . Map.toList $ valvesMap
    -- let openValves = 2^(Map.size valvesMap - 1) - 1

    -- print valvesMap
    print valveLabels
    -- print valvesMap'
    putStrLn $ genDotf valvesMap id
    let valveLabels' = Map.fromList . map (\(k,v) -> (v,k)) . Map.toList $ valveLabels
    putStrLn $ genDotf valvesMap' (valveLabels' !)

    -- print $ evalState (maxPressure valvesMap' [(30, valveLabels ! "AA")] openValves) Map.empty
    -- print $ evalState (maxPressure valvesMap' [(26, valveLabels ! "AA"), (26, valveLabels ! "AA")] openValves) Map.empty

data Valve a = Valve { label :: a, pressure :: Int, tunnels :: [Tunnel a] } deriving Show
type ValvesMap a = Map a (Valve a)
type ValveLabel = Int
type ValveSet = Int
data Tunnel a = Tunnel { length :: Int, valveLabel :: a } deriving Show

transformLabels :: (a -> b) -> Valve a -> Valve b
transformLabels f (Valve lbl p ts) = Valve (f lbl) p (map (\(Tunnel l lbl) -> Tunnel l (f lbl)) ts)

parseValve :: String -> Valve String
parseValve str =
    let [a, b] = splitOn "; " str
        label = take 2 . drop 6 $ a
        flowRate = read @Int . drop 23 $ a
        tunnels = map (Tunnel 1) . splitOn "," . filter (not . isSpace) . drop 22 $ b
    in Valve label flowRate tunnels

simplify :: ValvesMap String -> ValvesMap String
simplify valvesMap =
    let uselessValves = map label . filter (\v -> pressure v == 0 && label v /= "AA") . map snd . Map.toList $ valvesMap
    in foldl' removeValve valvesMap uselessValves

choose :: [a] -> [(a, [a])]
choose xs = map (\(xs, ys) -> (head ys, xs ++ tail ys)) $ init (zip (inits xs) (tails xs))

removeValve :: Ord a => Map a (Valve a) -> a -> Map a (Valve a)
removeValve vmap lbl =
    let valve = vmap ! lbl
        vmap1 = Map.delete lbl vmap
        vmap2 = foldl' (flip (Map.adjust (`removeTunnel` lbl)) ) vmap1 (map valveLabel (tunnels valve))
        ts = map (\(Tunnel l lbl, ts) -> (lbl, map (\(Tunnel l2 lbl2) -> Tunnel (l+l2) lbl2) ts)) . choose . tunnels $ valve
    in foldl' (\ vm (lbl', ts) -> Map.adjust (`addTunnels` ts) lbl' vm) vmap2 ts
  where
    addTunnels (Valve lbl p ts) tunnels = Valve lbl p (tunnels ++ ts)
    removeTunnel v lbl = v { tunnels = filter ((/= lbl) . valveLabel) (tunnels v) }

-- type MemoKey =  ([(Int, ValveLabel)], ValveSet)
-- type MemoState = State (Map MemoKey Int)

-- maxPressure :: ValvesMap ValveLabel -> [(Int, ValveLabel)] -> ValveSet -> MemoState Int
-- maxPressure _ [] _ = return 0
-- maxPressure vm ((t,_):xs) openValves | t <= 0 = maxPressure vm xs openValves
-- maxPressure vm xs openValves = do
--     cache <- get
--     let key = (sort xs, openValves)
--     if Map.member key cache then
--         return $ cache ! key
--     else do
--         let ((t,lbl), xs') = (\xs -> (head xs, tail xs)) . sort $ xs
--         let moves = [(pressure (vm ! lbl) * (t-1), t-1, lbl, clearBit openValves lbl) | testBit openValves lbl]
--                  ++ map (\ (Tunnel l nbr) -> (0, t - l, nbr, openValves)) (tunnels (vm ! lbl))
--         result <- maximum <$> mapM (\(p, t, lbl, vs) -> (+p) <$> maxPressure vm ((t, lbl):xs') vs) moves
--         modify (Map.insert key result)
--         return result

-- Map a (Valve a)
genDotf :: (Ord a, Show a) => ValvesMap a -> (a -> String) -> String
genDotf valvesMap labelFunc =
    genDotFile
        nodes
        labelFunc
        (\n -> map (\t -> (valveLabel t, 1)) (tunnels (valvesMap ! n)))
  where
    nodes = reverse $ Map.keys valvesMap
    -- edges = 

genDotFile :: Show a => [a] -> (a -> String) -> (a -> [(a, Int)]) -> String
genDotFile nodes labelFunc edgeFunc =
    "graph graphname {\n" ++
    unlines (map printNode nodes) ++
    unlines (map printEdges nodes) ++
    -- parseNodes nodes ++
    "}"
  where
    printNode n = show n ++ " [label=" ++ show (labelFunc n) ++ "]"
    printEdges n1 = unlines $ map (\(n2, w) -> printEdge n1 n2) (edgeFunc n1)
    printEdge n1 n2 = show n1 ++ " -- " ++ show n2 ++ " [label=" ++ show 1 ++ "]"
