import           Data.Function   (on)
import           Data.List       (maximumBy, minimumBy, sortOn, transpose, (\\))
import           Data.List.Split (chunksOf, splitOn)
import           Data.Map        (fromList, (!))

main :: IO ()
main = do
  input <- lines <$> readFile "2021/Day04/day04.txt"
  let draws = map (read @Int) . splitOn "," . head $ input
      drawsMap = fromList . sortOn snd $ zip draws [0 ..]
      boards = map (map (map (read @Int) . words) . drop 1) . chunksOf 6 . tail $ input
      results = zip (map (numDraws drawsMap) boards) boards
      (winningRound, winningBoard) = minimumBy (compare `on` fst) results
      (lastRound, lastBoard) = maximumBy (compare `on` fst) results
      sumWinner = sum $ concat winningBoard \\ take (winningRound + 1) draws
      sumLast = sum $ concat lastBoard \\ take (lastRound + 1) draws
  print (sumWinner * (draws !! winningRound))
  print (sumLast * (draws !! lastRound))
  where
    numDraws drawsMap board =
      minimum . map (maximum . map (drawsMap !)) $ (board ++ transpose board)
