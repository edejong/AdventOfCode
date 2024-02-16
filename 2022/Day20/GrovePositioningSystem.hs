{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
import qualified Data.Sequence as Seq
import Data.Sequence (Seq (..), (><))
import Data.Maybe (fromJust)

main :: IO ()
main = do
    xs <- map (read @Integer) . lines <$> readFile "2022/Day20/day20.txt"
    print $ decrypt 1 xs
    print $ decrypt 10 . map (*811589153) $ xs

decrypt :: Int -> [Integer] -> Integer
decrypt n xs =
    let mix' = (!!n) . iterate (mix 0)
        xs' = fmap snd . mix' . Seq.fromList . zip [0..] $ xs
        i = fromJust $ Seq.findIndexL (==0) xs'
     in sum $ map (Seq.index xs' . (`mod` length xs') . (+i)) [1000, 2000, 3000]

mix :: Int -> Seq (Int, Integer) -> Seq (Int, Integer)
mix i xs =
    case Seq.findIndexL ((==i) . fst) xs of
        Just i' ->
            let (ls, r :<| rs) = Seq.splitAt i' xs
                (ls2, rs2) = insertCyclic (snd r) r (ls, rs)
             in mix (i+1) (ls2 >< rs2)
        Nothing -> xs

insertAt :: Int -> a -> Seq a -> Seq a
insertAt i x xs
  | i >= 0 = Seq.insertAt i x xs
  | otherwise = Seq.insertAt (Seq.length xs + i) x xs

insertCyclic :: Integer -> a -> (Seq a, Seq a) -> (Seq a, Seq a)
insertCyclic i x (ls,rs)
    | i' < 0 = (insertAt i' x ls, rs)
    | otherwise = (ls, insertAt i' x rs)
  where
    lenL = fromIntegral $ Seq.length ls
    lenR = fromIntegral $ Seq.length rs
    len = lenL+lenR
    i' = fromIntegral $ ((i + lenL) `mod` len) - lenL
