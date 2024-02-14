{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
import           Data.Bifunctor     (bimap)
import           Data.Map           (Map, (!))
import qualified Data.Map           as M
import           Data.Maybe         (fromMaybe)
import           Data.Range
import           Text.Parsec
import           Text.Parsec.String

main :: IO ()
main = do
    Right (ws, ps) <- parseFromFile inputP "2023/Day19/day19.txt"
    let f = workflowFn (M.fromList [(n,cs) | WF n cs <- ws]) "in"
    print $ sum . map score' . concatMap f $ ps
    print $ sum . map numPossibilities . f $ mkAllParts
  where
    score' (Parts (SingletonRange x) (SingletonRange m) (SingletonRange a) (SingletonRange s)) = x+m+a+s
    score' _ = error "wut!"
    numPossibilities (Parts x m a s) = rangeSize x * rangeSize m * rangeSize a * rangeSize s
    rangeSize r = let (x, y) = rangeBounds r in y - x + 1
    rangeBounds (SpanRange (Bound x xType) (Bound y yType)) =
        (if xType == Inclusive then x else succ x,
        if yType == Inclusive then y else pred y)

inputP :: Parser ([Workflow], [Parts Integer])
inputP = (,) <$> manyTill (workflowP <* endOfLine) endOfLine <*> sepBy1 partsP endOfLine
  where
    workflowP = WF <$> identP <*> between (char '{') (char '}') conditionListP
    identP = many1 letter
    conditionListP = sepBy ifThenP (char ',')
    ifThenP =
        try ((\a b c d -> (Cond a b c, d)) <$> oneOf "xmas" <*> orderingP <*> integerP <*> (char ':' *> identP))
        <|> ((Otherwise,) <$> identP)
    orderingP = (LT <$ char '<') <|> (GT <$ char '>')
    integerP = read <$> many1 digit
    partsP = mkPart <$> (string "{x=" *> integerP) <*> (string ",m=" *> integerP) <*> (string ",a=" *> integerP) <*> ((string ",s=" *> integerP) <* string "}")

data Workflow = WF String [(Condition, String)] deriving Show
data Condition = Cond Char Ordering Integer | Otherwise deriving Show
data Parts t = Parts { x' :: Range t, m' :: Range t, a' :: Range t, s' :: Range t } deriving Show

workflowFn :: Map String [(Condition, String)] -> String -> Parts Integer -> [Parts Integer]
workflowFn _ "A" = (: [])
workflowFn _ "R" = const []
workflowFn m wf  = workflowFn' m (m ! wf)

workflowFn' :: Map String [(Condition, String)] -> [(Condition, String)] -> Parts Integer -> [Parts Integer]
workflowFn' m ((c,wf):cs) parts = do
    let (pTrue, pFalse) = splitParts' c parts
    let as = workflowFn m wf <$> pTrue
    let bs = workflowFn' m cs <$> pFalse
    fromMaybe [] $ mconcat [as,bs]

mkPart :: Integer -> Integer -> Integer -> Integer -> Parts Integer
mkPart x m a s = Parts (SingletonRange x) (SingletonRange m) (SingletonRange a) (SingletonRange s)

mkAllParts :: Parts Integer
mkAllParts = Parts (1 +=+ 4000) (1 +=+ 4000) (1 +=+ 4000) (1 +=+ 4000)

splitParts' :: Condition -> Parts Integer -> (Maybe (Parts Integer), Maybe (Parts Integer))
splitParts' (Cond c o n) parts = splitParts c o n parts
splitParts' Otherwise parts    = (Just parts, Nothing)

splitParts :: Char -> Ordering -> Integer -> Parts Integer -> (Maybe (Parts Integer), Maybe (Parts Integer))
splitParts c o n parts =
    case c of
        'x' -> let f x = (\x'' -> parts { x' = x'' }) <$> x in bimap f f (splitRange (x' parts) o n)
        'm' -> let f m = (\m'' -> parts { m' = m'' }) <$> m in bimap f f (splitRange (m' parts) o n)
        'a' -> let f a = (\a'' -> parts { a' = a'' }) <$> a in bimap f f (splitRange (a' parts) o n)
        's' -> let f s = (\s'' -> parts { s' = s'' }) <$> s in bimap f f (splitRange (s' parts) o n)

splitRange :: Ord a => Range a -> Ordering -> a -> (Maybe (Range a), Maybe (Range a))
splitRange r ordering x =
    let (a, b) = case ordering of {LT -> (ube x, lbi x); GT -> (lbe x, ubi x)}
    in (safeHead $ intersection [r] [a], safeHead $ intersection [r] [b])
  where
    safeHead = \case { [] -> Nothing; xs -> Just $ head xs }
