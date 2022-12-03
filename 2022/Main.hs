{-# LANGUAGE TypeApplications #-}
module Main where
import qualified Day01.CalorieCounting
import qualified Day02.RockPaperScissors
import qualified Day03.RucksackReorganization
-- !!aoc2022 import hook!! --
import System.Environment ( getArgs, getProgName )
import System.Exit ( die )

tmp :: [IO ()]
tmp =
  [ Day01.CalorieCounting.main
  , Day02.RockPaperScissors.main
  , Day03.RucksackReorganization.main
  -- !!aoc2022 main hook!! --
  ]

main :: IO ()
main = getArgs >>= parse >>= (tmp !!) . subtract 1

parse :: [String] -> IO Int
parse [day] = return $ (read @Int) day
parse _ = do
    progName <- getProgName
    die $ "Usage: " ++ progName ++ " <day>"