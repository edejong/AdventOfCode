module Day17.PyroclasticFlow where
import Control.Monad.State.Strict
    ( modify', MonadState(get), replicateM_, modify, evalState, State )
import Debug.Trace (trace, traceM)
import Data.Maybe (fromJust)
import Data.Set (Set)
import qualified Data.Set as S

main :: IO ()
main = do
    xs <- readFile "2022/data/day17.txt"
    let h1 = evalState (calcTowerHeight 2022) (mkState xs)
    let h2 = evalState (calcTowerHeight 1000000000000) (mkState xs)
    print (h1, h2)

shapes :: [Shape]
shapes = 
  [
    ["####"]

  , [ ".#."
    , "###"
    , ".#." ]

  , [ "..#"
    , "..#"
    , "###" ]

  , [ "#"
    , "#"
    , "#"
    , "#" ]

  , [ "##"
    , "##" ]
  ]

constTunnelWidth = 7
constStartX = 2
constStartHeight = 3

type Shape = [[Char]]
type Tunnel = [[Char]]
data Rock = Rock { x :: Int, shape :: Shape }
data MyState = MyState { maybeRock :: Maybe Rock, rockCount :: Int, tunnel :: Tunnel, jetPattern :: [Char], jetId :: Int, jetLen :: Int }

shapeWidth :: Shape -> Int
shapeWidth = length . head

shapeHeight :: Shape -> Int
shapeHeight = length

mkState :: [Char] -> MyState
mkState jetPattern = MyState Nothing 0 [] (cycle jetPattern) 0 (length jetPattern)

nextShape :: MyState -> Int
nextShape s = rockCount s `mod` 5

calcTowerHeight :: Int -> State MyState Int
calcTowerHeight n = do
  (cycleJetId, cycleShape) <- findCycle' 1 S.empty
  s1 <- get
  spawnAndDropRocksUntil (\s -> jetId s == cycleJetId && nextShape s == cycleShape)
  s2 <- get
  let cycleLength = rockCount s2 - rockCount s1
      cycleHeight = towerHeight s2 - towerHeight s1
  rocksRemaining <- (n-) . rockCount <$> get
  let cycles = rocksRemaining `div` cycleLength
  replicateM_ (rocksRemaining `mod` cycleLength) (spawnRock >> dropRock)
  (+ (cycles * cycleHeight)) . towerHeight <$> get
  where
    towerHeight = length . trimEmpty . tunnel
    findCycle' n set = do
      spawnRock >> dropRock
      s <- get
      let tmp = (jetId s, nextShape s)
      if tmp `S.member` set then return tmp else findCycle' (n + 1) (S.insert tmp set)

spawnAndDropRocksUntil :: (MyState -> Bool) -> State MyState ()
spawnAndDropRocksUntil f = do
  spawnRock >> dropRock
  s <- get
  if f s then return () else spawnAndDropRocksUntil f

spawnAndDropRocks :: Int -> State MyState ()
spawnAndDropRocks n = do
  replicateM_ n (spawnRock >> dropRock)

spawnRock :: State MyState ()
spawnRock = do
  nextShapeId <- nextShape <$> get
  let shape = shapes !! nextShapeId
  let rock = Rock constStartX shape
  tunnel <- addEmpty (shapeHeight shape + constStartHeight) . trimEmpty . tunnel <$> get
  modify' (\s -> s { maybeRock = Just rock, rockCount = rockCount s + 1, tunnel = tunnel })
  traceState "\nRock begins falling:"

dropRock :: State MyState ()
dropRock = do
  pushRock
  rock <- fromJust . maybeRock <$> get
  tnl <- tunnel <$> get
  if collides rock (tail tnl) then do
    modify' (\s -> s { tunnel = merge rock tnl '#' })
    traceState "\nRock falls 1 unit, causing it to come to rest:"
  else do
    modify' (\s -> s { tunnel = tail tnl } )
    traceState "\nRock falls 1 unit:"
    dropRock
    tnl' <- tunnel <$> get
    modify' (\s -> s { tunnel = head tnl : tnl' } )

pushRock :: State MyState ()
pushRock = do
  j <- readJetPattern
  tunnel <- tunnel <$> get
  (Rock x shape) <- fromJust . maybeRock <$> get
  let x' = x + (if j == '<' then (-1) else 1)
  let rock' = Rock x' shape
  let dir = if j == '<' then "left" else "right"
  if collides rock' tunnel then do
    traceState $ "\nJet of gas pushes rock " ++ dir ++ ", but nothing happens:"
  else do
    modify' (\s -> s { maybeRock = Just rock' })
    traceState $ "\nJet of gas pushes rock " ++ dir ++ ":"

readJetPattern :: State MyState Char
readJetPattern = do
  jp <- jetPattern <$> get
  modify (\s -> s { jetPattern = tail jp, jetId = (jetId s + 1) `mod` jetLen s } )
  return (head jp)

trimEmpty :: Tunnel -> Tunnel
trimEmpty = dropWhile $ all (== '.')

addEmpty :: Int -> Tunnel -> Tunnel
addEmpty n tunnel = replicate n "......." ++ tunnel

collides :: Rock -> Tunnel -> Bool
collides (Rock x shape) tunnel
    | not (x >= 0 && x <= rockMaxX) = True
    | otherwise = any (uncurry collidesRow) (zip shape tunnel')
  where
    rockMaxX = constTunnelWidth - shapeWidth shape
    tunnel' = map (drop x) (tunnel <> ["-------"])
    collidesRow rs ts = any (\(r, t) -> r /= '.' && t /= '.') (zip rs ts)

merge :: Rock -> Tunnel -> Char -> Tunnel
merge (Rock x (rs:rss)) (ts:tss) c = mergeRow c x rs ts : merge (Rock x rss) tss c
  where
    mergeRow c 0 (r:rs) (t:ts) = (if r /= '.' then c else t) : mergeRow c 0 rs ts
    mergeRow c n rs (t:ts) = t : mergeRow c (n-1) rs ts
    mergeRow _ _ [] ts = ts
    mergeRow _ _ _ [] = []
merge _ tss _ = tss

traceState :: String -> State MyState ()
traceState msg = return () -- traceM msg >> get >>= traceM . drawState

drawState :: MyState -> String
drawState (MyState maybeRock _ tunnel _ _ _) = drawTunnel maybeRock tunnel

drawTunnel :: Maybe Rock -> Tunnel -> String
drawTunnel maybeRock tunnel =
  let tunnel' = maybe tunnel (\rock -> merge rock tunnel '@') maybeRock
  in unlines tunnel'
