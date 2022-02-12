{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
module Day23.Amphipod where
import Data.Char
import qualified Data.HashPSQ as PSQ
import Data.List
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as S
import GHC.Generics --(Generic)
import Data.Hashable
import Debug.Trace (trace)
import Data.Bifunctor (Bifunctor(first))

data FrontOrBack = Front | Back deriving (Enum, Eq, Generic, Ord, Show)
data BurrowPos = Hallway { _hallwayPos::Int } | Room { _roomNo::Int, _roomPos::FrontOrBack } deriving (Eq, Generic, Ord, Show)
data AmphipodType = A | B | C | D deriving (Enum, Eq, Generic, Ord, Read, Show)
data Amphipod = Amphipod { _amphipodType::AmphipodType, _pos::BurrowPos } deriving (Eq, Generic, Ord, Show)
type Amphipods = Set Amphipod
type Cost = Integer
type HallwayState = [(Int, AmphipodType)]
type SideRoomState = [AmphipodType]
type BurrowState = (HallwayState, [SideRoomState])

type MinHeap = PSQ.HashPSQ Amphipods Cost ()

data DoorInfo = DoorInfo { canEnter::Bool, openHallwayRange::(Int, Int) } deriving Show

instance Hashable AmphipodType
instance Hashable Amphipod
instance Hashable BurrowPos
instance Hashable FrontOrBack

--  0123456789a
--  ........... 
--    B C B D   
--    A D C A   
-- r: 0 1 2 3

main :: IO ()
main = do
    b <- readBurrow <$> readFile "2021/data/day23.txt"
    -- let b = readBurrow "\n...........\nABCD\nABCD"
    let tmp = minimalCost b
    print tmp
    -- let tmp = validMoves $ (S.toList b)
    -- putStr $ unlines . map (\ (cost, as) -> show cost ++ "\n" ++ showBurrow as) $ tmp

finished :: Amphipods
finished = readBurrow "\n...........\nABCD\nABCD"

reducePrio :: Cost -> Amphipods -> MinHeap -> MinHeap
reducePrio prio key h = snd $ PSQ.alter f key h
  where f (Just (p, v)) = ((), Just (min p prio, v))
        f Nothing = ((), Just (prio, ()))

minimalCost :: Amphipods -> Cost
minimalCost as = minimalCost' (PSQ.singleton as 0 ()) S.empty

minimalCost' :: MinHeap -> Set Amphipods -> Cost
minimalCost' h visited
  | trace (show cost ++ "\n" ++ showBurrow as ++ "\n") False = 0
  | as == finished = cost
  | otherwise = minimalCost' h'' visited'
  where
    (as, cost, _, h') = fromJust . PSQ.minView $ h
    h'' = foldr f h' moves
    visited' = S.insert as visited
    moves = map (first (+cost)) . filter (flip S.notMember visited . snd) . validMoves . S.toList $ as
    f (p, k) = reducePrio p k

validMoves :: [Amphipod] -> [(Cost, Amphipods)]
validMoves as = concatMap moveFirst . rotations $ as
  where st = mkBurrowState as
        moveFirst (x:xs) = map (\ (cost, pos) -> (cost, S.fromList $ x { _pos=pos } : xs)) . validMovesForAmphipod st (doorInfo st) $ x
        moveFirst _ = undefined

validMovesForAmphipod :: BurrowState -> [DoorInfo] -> Amphipod -> [(Cost, BurrowPos)]
validMovesForAmphipod (hallway, rooms) doors (Amphipod t (Hallway p))
  | canEnter door && canReach = [(numSteps * cost t, Room r frontOrBack)]
  where
    r = fromEnum t
    canReach = (\ (l, r) -> p `elem` [l-1..r+1]) . openHallwayRange $ door
    door = doors !! r
    roomPos = r * 2 + 2
    frontOrBack = if null (rooms !! r) then Back else Front
    numSteps = fromIntegral $ abs (p - roomPos) + fromEnum frontOrBack + 1
validMovesForAmphipod (hallway, rooms) doors (Amphipod t (Room r frontOrBack))
  | canExit = [(numSteps x * cost t, Hallway x) | let (l, r) = openHallwayRange door, x <- [l..r] \\ [2,4..8]]
  where
    canExit = frontOrBack == Front || length room == 1
    door = doors !! r
    room = rooms !! r
    roomPos = r * 2 + 2
    numSteps p = fromIntegral $ abs (p - roomPos) + fromEnum frontOrBack + 1
validMovesForAmphipod _ _ _ = []

cost :: AmphipodType  -> Integer
cost = (10 ^) . fromEnum

mkBurrowState :: [Amphipod] -> BurrowState
mkBurrowState as = (hallwayState, rooms)
  where
    hallwayState = sort . mapMaybe (\case { Amphipod t (Hallway pos) -> Just (pos, t); _ -> Nothing }) $ as
    inRoom = mapMaybe (\case { Amphipod t (Room r _) -> Just (r, t); _ -> Nothing }) as
    rooms = [map snd . filter (\x -> fst x == r) $ inRoom | r <- [0..3], let a = toEnum r :: AmphipodType]

doorInfo :: BurrowState -> [DoorInfo]
doorInfo (hallwayState, rooms) = zipWith DoorInfo canEnter hallwayOpenRangePerDoor
  where
    hallwayOccupied = [-1] ++ map fst hallwayState ++ [11]
    hallwayOpenRanges = filter (uncurry (<=)) . zipWith (\ a b -> (a + 1, b - 1)) hallwayOccupied $ tail hallwayOccupied
    hallwayOpenRangePerDoor = [fromJust . find (\ (l, r) -> x `elem` [l..r]) $ hallwayOpenRanges | x <- [2,4..8]]
    canEnter = [all (==a) r | (a, r) <- zip [A, B, C, D] rooms]

---------- Util

rotations :: [a] -> [[a]]
rotations xs = init . zipWith (flip (++)) (inits xs) $ tails xs

---------- Debug

isHallway :: BurrowPos -> Bool
isHallway (Hallway _) = True
isHallway _ = False

showBurrow :: Amphipods -> String
showBurrow as = hallway ++ "\n " ++ rooms Front ++ "\n " ++ rooms Back ++ "\n"
  where
    hallway = buildStr 11 0 . sort $ [(pos, t) | a@(Amphipod t p) <- S.toList as, isHallway p, let (Hallway pos) = p]
    buildStr w i _ | w == i= ""
    buildStr w i ((pos, t):xs)
      | i == pos = show t ++ buildStr w (i+1) xs
    buildStr w i xs = '.' : buildStr w (i+1) xs
    inRooms frontOrBack = concatMap (\case { (Amphipod t (Room r f)) | f == frontOrBack -> [(r, t)]; _ -> []}) $ as
    rooms frontOrBack = concatMap ((\a b -> [a, b]) ' ') . buildStr 4 0 . sort $ inRooms frontOrBack

readBurrow :: String -> Amphipods
readBurrow s = S.fromList xs
  where
    ls = lines s
    isPos c = c == '.' || isAlpha c
    readPositions = filter (\ (i, c) -> isAlpha c) . zip [0..] . filter isPos
    xs = concat . zipWith (\line str -> map (toAmphipod line) . readPositions $ str) [1..3] . drop 1 $ ls
    toAmphipod line (i, c) = Amphipod (read [c]) pos
      where pos = case line of 1 -> Hallway i
                               2 -> Room i Front
                               3 -> Room i Back
                               _ -> undefined
