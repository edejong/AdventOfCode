{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# LANGUAGE TupleSections #-}
module Day16.ProboscideaVolcanium where
import Data.Map ( Map, (!) )
import qualified Data.Map as Map
import Data.List.Split (splitOn)
import Data.List ((\\), delete, foldl')
import Data.Char (isDigit, isAlpha, isSpace)
import Control.Monad.State
import Data.Int (Int64)
import Data.Bits (testBit, Bits (clearBit), setBit)

main :: IO ()
main = do
    valves <- map parseValve . lines <$> readFile "2022/data/day16.txt"
    let valveLabels = Map.fromList . flip zip [0..] . map (\(a,_,_)->a) $ valves
        valves' = Map.fromList . map (\(lbl,p,nbrs) -> (valveLabels ! lbl, Valve p (map (valveLabels !) nbrs))) $ valves
        openValves = foldl' setBit (0::Int64) . map fst . filter ((>0) . pressure . snd) . Map.toList $ valves'
    print $ evalState (maxPressure valves' (valveLabels ! "AA") 30 openValves) Map.empty

data Valve = Valve { pressure :: Int, neighbours :: [ValveLabel] } deriving Show
type Valves = Map ValveLabel Valve
type ValveLabel = Int
type ValveSet = Int64

parseValve :: String -> (String, Int, [String])
parseValve str =
    let [a, b] = splitOn "; " str
        label = take 2 . drop 6 $ a
        flowRate = read @Int . drop 23 $ a
        neighbours = splitOn "," . filter (not . isSpace) . drop 22 $ b
    in (label, flowRate, neighbours)

type MemoKey =  (ValveLabel, Int, ValveSet)
type MemoState = State (Map MemoKey Int)

maxPressure' :: Valves -> ValveLabel -> Int -> ValveSet -> MemoState Int
maxPressure' valvesMap = memoized (maxPressure valvesMap)

maxPressure :: Valves -> ValveLabel -> Int -> ValveSet -> MemoState Int
maxPressure _ _ 0 _ = return 0
maxPressure valvesMap valveLabel timeRemaining openValves = do
    let moves = tmp valve valveLabel
    moves' <- mapM (\(p, lbl) -> (+p) <$> maxPressure' valvesMap lbl timeRemaining' (if p == 0 then openValves else clearBit openValves lbl)) moves
    return $ maximum moves'
  where
    timeRemaining' = timeRemaining - 1
    valve = valvesMap ! valveLabel
    releasedPressure = pressure valve * timeRemaining'
    tmp v lbl = [(releasedPressure, lbl) | testBit openValves lbl] ++ map (0,) (neighbours v)


memoized :: (ValveLabel -> Int -> ValveSet -> MemoState Int) -> ValveLabel -> Int -> ValveSet -> MemoState Int
memoized f valveLabel timeRemaining openValves = do
    cache <- get
    let key = (valveLabel, timeRemaining, openValves)
    if Map.member key cache then
        return $ cache ! key
    else do
        result <- f valveLabel timeRemaining openValves
        modify (Map.insert key result)
        return result




-- doStep [posA, posB] timeRemaining openValves =
--     [nextA <- ["AA", "BB", "CC"], nbrsB <- ["BB", "DD"]]

-- >>> xs = [1,2]
-- >>> ys =  [3,4,5]
-- >>> zs = [6,7]
-- >>> foldl' (\as bs -> [(b:a) | a <- as, b <- bs]) ([[]]) [zs, ys,xs]
-- [[1,3,6],[2,3,6],[1,4,6],[2,4,6],[1,5,6],[2,5,6],[1,3,7],[2,3,7],[1,4,7],[2,4,7],[1,5,7],[2,5,7]]

