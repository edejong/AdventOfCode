{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

import Data.List (iterate', foldl')
import qualified Data.Map as M
import Data.Map (Map, (!))
import qualified Data.Sequence as Seq
import Data.Sequence (Seq ((:<|)))
import Text.Parsec ( letter, spaces, string, many1, sepBy1, (<|>) )
import Text.Parsec.String (Parser, parseFromFile)

main :: IO ()
main = do
    xs <- either (error . show) id <$> parseFromFile inputP "2023/Day20/day20.txt"
    let outputs = [(o, (Output, [])) | o <- ["output", "rx"]]
    let cfg = M.fromList $ outputs ++ [(src, (t, dsts)) | (t, src, dsts) <- xs]
    let st0 = mkStateWithCounts . mkSystemState $ xs
    let st1 = head . dropWhile ((<1000) . fst . (! "button") . counters) . iterate' (pushButton cfg) $ st0
    print $ uncurry (*) . (! "totals") . counters $ st1

    let monitorSrc = M.keys $ (! "cl") . st $ st0
    let f src = head . dropWhile ((<1) . snd . (! src) . counters) . iterate' (pushButton cfg) $ st0
    print $ lcm' . map ((fst . (! "button") . counters) . f) $ monitorSrc
  where
    lcm' (x:xs) = foldl' lcm x xs

data ModuleType = Broadcaster | Flipflop | Conjunction | Output deriving (Eq, Show)
type ModuleName = String
data Pulse = Pulse ModuleName Bool ModuleName deriving Show

type ModuleConfig = Map ModuleName (ModuleType, [ModuleName])
type ModuleState = Map String Bool
type SystemState = Map ModuleName ModuleState

type PulseCounter = (Integer, Integer)
-- In retrospect it would be nicer to just keep track of a Pulse log instead of
-- awkwardly keeping counters.
data StateWithCounts = ST { counters :: Map String PulseCounter, st :: SystemState } deriving Show

inputP :: Parser [(ModuleType, ModuleName, [ModuleName])]
inputP = sepBy1 edgeP spaces
  where
    edgeP = (,,) <$> moduleTypeP <*> identP <*> (string " -> " *> sepBy1 identP (string ", "))
    moduleTypeP = (Flipflop <$ string "%") <|> (Conjunction <$ string "&") <|> (Broadcaster <$ string "")
    identP = many1 letter

mkSystemState :: [(ModuleType, ModuleName, [ModuleName])] -> SystemState
mkSystemState xs =
    let cs = M.fromList [(src, M.empty) | (t, src, _) <- xs, t == Conjunction]
        cs' = Data.List.foldl' (\memo (src,dst) -> M.adjust (M.insert src False) dst memo) cs [(src,dst) | (_, src, dsts) <- xs, dst <- dsts]
        os = M.fromList [(dst, M.singleton "" False) | (_, _, dsts) <- xs, dst <- dsts, dst `M.notMember` cs]
    in M.unions [cs', os]

mkStateWithCounts :: SystemState -> StateWithCounts
mkStateWithCounts s = ST (M.fromList [(x,(0,0)) | x <- ["button", "totals"] ++ monitorSrc]) s
  where
    monitorSrc = M.keys $ (! "cl") s

pushButton :: ModuleConfig -> StateWithCounts -> StateWithCounts
pushButton cfg = processQueue cfg (Seq.singleton (Pulse "button" False "broadcaster"))

processQueue :: ModuleConfig -> Seq Pulse -> StateWithCounts -> StateWithCounts
processQueue _ Seq.Empty s = s
processQueue cfg (pulse :<| queue) (ST c s) = do
    let monitorSrc = M.keys $ (! "cl") s
    let c' = foldl' (\c'' nm -> updateSrc nm pulse c'') (updateTotal pulse c) ("button":monitorSrc)
    let (st', ps) = processPulse cfg pulse s
    processQueue cfg (queue <> Seq.fromList ps) (ST c' st')
  where
    updateCounter False (x, y) = (x+1, y)
    updateCounter True (x, y) = (x, y+1)
    updateTotal (Pulse _ val _) = M.adjust (updateCounter val) "totals"
    updateSrc n (Pulse src val _) | n == src = M.adjust (updateCounter val) src
    updateSrc _ _ = id

processPulse :: ModuleConfig -> Pulse -> SystemState -> (SystemState, [Pulse])
processPulse cfg (Pulse src val name) s =
    let (t, dsts) = cfg ! name
        input = if t == Conjunction then src else ""
        updateState x = setModuleState name input x s
        st' = case (t, val) of
            (Flipflop, False) -> updateState (not (getModuleState name s))
            (Conjunction, _) -> updateState val
            (Output, _) -> updateState val
            _ -> s
        newVal = getModuleState name st'
        ps = case (t, val) of
            (Flipflop, False) -> bcast dsts newVal
            (Conjunction, _) -> bcast dsts (not newVal)
            (Broadcaster, _) -> bcast dsts val
            _ -> []
     in (st', ps)
  where
    getModuleState name' = and . (! name')
    setModuleState name' input val' = M.adjust (M.insert input val') name'
    bcast dsts val' = [Pulse name val' dst | dst <- dsts]
