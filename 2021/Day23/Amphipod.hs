module Day23.Amphipod where
import Data.Char ( ord, chr, isAlpha )
import Data.List ( find, (\\), transpose )
import Data.Maybe ( fromJust )
import Data.Bifunctor ( Bifunctor(second, first) )
import qualified Data.HashPSQ as PSQ
import qualified Data.Set as S
import Data.Set (Set)
import GHC.Generics (Generic)
import Data.Hashable ( Hashable )

type MinHeap = PSQ.HashPSQ Burrow Cost ()
type Cost = Int

type Burrow = (String, [String])

main :: IO ()
main = do
    f <- readFile "2021/data/day23.txt"
    let part1 = readBurrow f
    let part2 = readBurrow . unlines . insertAt 3 "#D#C#B#A#\n#D#B#A#C#" . lines $ f
    print $ uncurry minimalCost part1
    print $ uncurry minimalCost part2

isFinished :: Burrow -> Bool
isFinished b@(hallway, rooms) = all (=='.') hallway && all (==True) [all (==a) room | (a, room) <- zip ['A'..] rooms]

insertOrReducePrio :: Cost -> Burrow -> MinHeap -> MinHeap
insertOrReducePrio prio key h = snd $ PSQ.alter f key h
  where f (Just (p, v)) = ((), Just (min p prio, v))
        f Nothing = ((), Just (prio, ()))

minimalCost :: Int -> Burrow -> Cost
minimalCost roomSize b = minimalCost' roomSize (PSQ.singleton b 0 ()) S.empty

minimalCost' :: Int -> MinHeap -> Set Burrow -> Cost
minimalCost' roomSize h visited
  | isFinished b = cost
  | otherwise = minimalCost' roomSize h'' visited'
  where
    (b, cost, _, h') = fromJust . PSQ.minView $ h
    h'' = foldr f h' moves'
    visited' = S.insert b visited
    moves' = map (first (+cost)) . filter (flip S.notMember visited . snd) . moves roomSize $ b
    f (p, k) = insertOrReducePrio p k

moves :: Int -> Burrow -> [(Cost, Burrow)]
moves roomSize b@(hallway, rooms) = movesIntoHallway ++ movesIntoRooms
  where
    hallwayOccupied         = (\ xs -> [-1] ++ xs ++ [11]) . map fst . filter (isAlpha . snd) . zip [0..] $ hallway
    hallwayOpenRanges       = filter (uncurry (<=)) . zipWith (\ l r -> (l+1, r-1)) hallwayOccupied $ tail hallwayOccupied
    hallwayOpenRangePerDoor = [fromJust . find (\ (l, r) -> x `elem` [l..r]) $ hallwayOpenRanges | x <- [2,4..8]]
    movesIntoHallway        = [move roomSize b r x | (r, room, range) <- unemptyRooms, x <- [fst range..snd range] \\ [2,4..8]]
    movesIntoRooms          = [move roomSize b r x | (x, a) <- inHallway, let r = targetRoom a, canEnter r && canReach r x]
    unemptyRooms            = filter (\ (_, room, _) -> (not . null) room) . zip3 [0..] rooms $ hallwayOpenRangePerDoor
    inHallway               = filter (isAlpha . snd) . zip [0..] $ hallway
    canEnter r              = let a = targetAmphipod r in all (== a) (rooms !! r)
    canReach r pos          = let (l', r') = hallwayOpenRangePerDoor !! r in pos `elem` [l'-1..r'+1]
    targetAmphipod          = chr . (+ ord 'A')

move :: Int -> Burrow -> Int -> Int -> (Cost, Burrow)
move roomSize b@(hallway, rooms) r pos = (cost', (hallway', rooms'))
  where
    toHallway = hallway !! pos == '.'
    room = rooms !! r
    (a, n, c, f) = if toHallway then (head room, 1, a, tail) else (hallway !! pos, 0, '.', (a:))
    doorPos = 2 * r + 2
    numSteps = abs (pos - doorPos) + roomSize - length room + n
    cost' = ((10 ^) . targetRoom) a * numSteps
    hallway' = adjust (const c) pos hallway
    rooms' = adjust f r rooms

targetRoom :: Char -> Int
targetRoom = subtract (ord 'A') . ord

readBurrow :: String -> (Int, Burrow)
readBurrow = (\ (h:ds) -> (length ds, (h, map (filter isAlpha) . transpose $ ds))) . filter (not . null) . map (filter f) . lines
  where
    f c = isAlpha c || c == '.'

adjust :: (a -> a) -> Int -> [a] -> [a]
adjust f i = uncurry (++) . second (\(x:xs) -> f x : xs) . splitAt i

insertAt :: Int -> a -> [a] -> [a]
insertAt k x = (\ (l, r) -> l ++ [x] ++ r) . splitAt k