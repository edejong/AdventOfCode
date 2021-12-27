module Day18.Snailfish where
import qualified Control.Applicative as A ((<|>))
import Data.Char (digitToInt)
import Text.Parsec (digit, string, (<|>), between, char, parse, ParseError)
import Text.Parsec.String (Parser)
import Data.Either (fromRight)
import Data.List (find, foldl', unfoldr, tails)
import Data.Maybe (fromJust, fromMaybe)

main :: IO ()
main = do
    Right xs <- mapM fromString . lines <$> readFile "2021/data/day18.txt"
    print $ snailMagnitude . snailSum $ xs
    print $ maximum . map (snailMagnitude . uncurry snailAdd) . pairs $ xs

------------- Parser
snailNum = num <|> pair
pair = between (string "[") (string "]") pair'
pair' = Pair <$> snailNum <*> (char ',' *> snailNum)
num = Num . digitToInt <$> digit

------------- Helper Functions
converge :: Eq b => (b -> b) -> b -> b
converge f x = fst . fromJust . find (uncurry (==)) $ zip xs (tail xs)
  where xs = iterate f x

pairs :: [a] -> [(a, a)]
pairs l = concat [[(x,y), (y,x)] | (x:ys) <- tails l, y <- ys]

------------- SnailFish Numbers
data SnailNum = Num Int | Pair SnailNum SnailNum deriving Eq

instance Show SnailNum where
  show (Num n) = show n
  show (Pair a b) = "[" ++ show a ++ "," ++ show b ++ "]"

-- Would be nicer to create a Read instance
fromString :: String -> Either ParseError SnailNum
fromString = parse snailNum ""

addLeft :: Int -> SnailNum -> SnailNum
addLeft 0 n = n
addLeft x (Num v) = Num (x + v)
addLeft x (Pair l r) = Pair (addLeft x l) r

addRight :: Int -> SnailNum -> SnailNum
addRight 0 n = n
addRight x (Num v) = Num (x + v)
addRight x (Pair l r) = Pair l (addRight x r)

reduce :: SnailNum -> SnailNum
reduce = converge (trySplit . converge explode)

explode :: SnailNum -> SnailNum
explode n = maybe n (\(x,_,_) -> x) (explode' 0 n)

explode' :: Int -> SnailNum -> Maybe (SnailNum, Int, Int)
explode' depth (Pair (Num a) (Num b)) | depth >= 4 = Just (Num 0, a, b)
explode' depth (Pair l r) =
        (\(l', ll, lr) -> (Pair l' (addLeft lr r), ll, 0))  <$> explode' (depth+1) l
  A.<|> (\(r', rl, rr) -> (Pair (addRight rl l) r', 0, rr)) <$> explode' (depth+1) r
explode' depth _ = Nothing

trySplit :: SnailNum -> SnailNum
trySplit n = fromMaybe n (trySplit' n)

trySplit' :: SnailNum -> Maybe SnailNum
trySplit' (Num x) | x > 9 = Just $ Pair (Num $ x `div` 2) (Num $ x - x `div` 2)
trySplit' (Pair l r) = (Pair <$> trySplit' l <*> pure r) A.<|> (Pair l <$> trySplit' r)
trySplit' _ = Nothing

snailAdd :: SnailNum -> SnailNum -> SnailNum
snailAdd a b = reduce $ Pair a b

snailSum :: [SnailNum] -> SnailNum
snailSum xs = foldl' snailAdd (head xs) (tail xs)

snailMagnitude :: SnailNum -> Int
snailMagnitude (Num x) = x
snailMagnitude (Pair l r) = 3 * snailMagnitude l + 2 * snailMagnitude r
