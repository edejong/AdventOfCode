import           Data.ByteString.Lazy.Char8 (append, pack, readFile)
import           Data.Digest.Pure.MD5       (md5)
import           Data.List                  (find)
import           Prelude                    hiding (concat, readFile)

main :: IO ()
main = do
    xs <- readFile "2015/Day04/day04.txt"
    print $ findMd5WithNZeroes xs 5
    print $ findMd5WithNZeroes xs 6
  where findMd5WithNZeroes xs n = find ((==replicate n '0') . take n . show . md5 . append xs . pack . show) [0..]
