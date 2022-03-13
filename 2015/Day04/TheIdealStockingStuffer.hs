module Day04.TheIdealStockingStuffer where
import Data.Digest.Pure.MD5 ( md5 )
import Data.ByteString.Lazy.Char8 ( readFile, pack, append )
import Prelude hiding (concat, readFile)
import Data.List (find)

main :: IO ()
main = do
    xs <- readFile "2015/data/day04.txt"
    print $ findMd5WithNZeroes xs 5
    print $ findMd5WithNZeroes xs 6
  where findMd5WithNZeroes xs n = find ((==replicate n '0') . take n . show . md5 . append xs . pack . show) [0..]