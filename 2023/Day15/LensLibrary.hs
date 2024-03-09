{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
import           Data.Char       (isAlpha, ord)
import           Data.Foldable   (toList)
import           Data.List       (foldl')
import           Data.List.Split (splitOn)
import           Data.Sequence   (Seq)
import qualified Data.Sequence   as S
import           Data.Word       (Word8)

main :: IO ()
main = do
  xs <- splitOn "," <$> readFile "2023/Day15/day15.txt"
  print $ sum $ map hash xs
  let boxes = foldl' (flip doOp) (S.replicate 256 []) (map parseOp xs)
  print $ sum . zipWith (\a -> (* a) . sum . zipWith (*) [1 ..] . map snd) [1 ..] . toList $ boxes

hash :: String -> Int
hash = fromIntegral . foldl' (\x -> (* 17) . (+ x)) (0 :: Word8) . map (fromIntegral . ord)

type Lens = (String, Int)
type Boxes = Seq [Lens]
data Op = Remove String | Insert Lens deriving (Show)

parseOp :: String -> Op
parseOp xs =
  let (as, bs) = span isAlpha xs
   in case bs of
        '=' : n -> Insert (as, read n)
        "-"     -> Remove as

doOp :: Op -> Boxes -> Boxes
doOp (Insert lens) = insertLens lens (hash . fst $ lens)
doOp (Remove lbl)  = removeLens lbl (hash lbl)

insertLens :: Lens -> Int -> Boxes -> Boxes
insertLens l@(lbl, _) = S.adjust insert
  where
    insert ls =
      let (as, bs) = span ((/= lbl) . fst) ls
       in as ++ (l : drop 1 bs)

removeLens :: String -> Int -> Boxes -> Boxes
removeLens lbl = S.adjust (filter ((/= lbl) . fst))
