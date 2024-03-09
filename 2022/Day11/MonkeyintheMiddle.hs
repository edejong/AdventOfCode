import           Data.List       (foldl', sortBy)
import           Data.List.Split (splitOn)
import           Data.Map        (Map, (!))
import qualified Data.Map        as M
import           Data.Ord        (Down (..), comparing)

data Monkey = Monkey MonkeyId MonkeyOp MonkeyTest deriving Show
type MonkeyId = Int
data MonkeyOp = Add Int | Mult Int | Square deriving Show
data MonkeyTest = MonkeyTest Int MonkeyId MonkeyId deriving Show

main :: IO ()
main = do
    xs <- map parseMonkey . splitOn "\n\n" <$> readFile "2022/Day11/day11.txt"
    let monkeys = map fst xs
        divisorsProduct = product . map ((\(MonkeyTest x _ _) -> x) . (\(Monkey _ _ t) -> t)) $ monkeys
        itemsMap = M.fromList . map (\(Monkey id' _ _, items) -> (id', items)) $ xs
        mb = monkeyBusiness monkeys itemsMap
    print (mb (`div` 3) 20, mb (`mod` divisorsProduct) 10000)
  where
    doRounds ms itemsMap worryFunc numRounds = iterate (doRound worryFunc ms) (M.empty, itemsMap) !! numRounds
    monkeyBusiness ms itemsMap worryFunc numRounds = product . take 2 . sortBy (comparing Down) . map snd . M.toList . fst $ doRounds ms itemsMap worryFunc numRounds

parseMonkey :: String -> (Monkey, [Int])
parseMonkey str = do
    let ls = lines str
        id' = read @Int .  init . drop 7 . head $ ls
        items = map (read @Int) . splitOn "," . drop 18 $ ls !! 1
        op = parseOperation . drop 23 $ ls !! 2
        test = parseTest . drop 3 $ ls
    (Monkey id' op test, items)

parseOperation :: String -> MonkeyOp
parseOperation "* old" = Square
parseOperation str =
    let x = read @Int (drop 2 str)
    in if head str == '+' then Add x else Mult x

parseTest :: [String] -> MonkeyTest
parseTest lines' = do
    let divisor = read @Int . drop 21 $ head lines'
        monkey1 = read @Int . drop 29 $ lines' !! 1
        monkey2 = read @Int . drop 30 $ lines' !! 2
    MonkeyTest divisor monkey1 monkey2

-- TODO: Should use Reader + State monad
doRound :: (Int -> Int) -> [Monkey] -> (Map MonkeyId Int, Map MonkeyId [Int]) -> (Map MonkeyId Int, Map MonkeyId [Int])
doRound worryFunc monkeys (countMap, itemsMap) = foldl' (f worryFunc) (countMap, itemsMap) monkeys
  where
    f worryFunc' (countMap', itemsMap0) monkey@(Monkey id' _ _) = do
        let items = itemsMap0 ! id'
            itemsMap1 = M.insert id' [] itemsMap0
            tmp = turn worryFunc' items monkey
            itemsMap2 = M.unionWith (<>) itemsMap1 tmp
            countMap'' = M.insertWith (+) id' (length items) countMap'
        (countMap'', itemsMap2)

turn :: (Int -> Int) -> [Int] -> Monkey -> Map MonkeyId [Int]
turn worryFunc items (Monkey _ op test) = M.fromListWith (<>) $
    let items' = map (worryFunc . doOperation op) items
    in zip (map (doTest test) items') (map (: []) items')

doOperation :: MonkeyOp -> Int -> Int
doOperation (Add n)  = (+ n)
doOperation (Mult n) = (* n)
doOperation Square   = \x -> x * x

doTest :: MonkeyTest -> (Int -> MonkeyId)
doTest (MonkeyTest divisor monkey1 monkey2) x = if x `mod` divisor == 0 then monkey1 else monkey2
