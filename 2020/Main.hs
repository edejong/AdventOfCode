{-# LANGUAGE TypeApplications #-}
module Main where
import qualified Day01.ReportRepair
import qualified Day02.PasswordPhilosophy
import qualified Day03.TobogganTrajectory
import qualified Day04.PassportProcessing
import qualified Day05.BinaryBoarding
import qualified Day06.CustomCustoms
import qualified Day07.HandyHaversacks
-- !!aoc2020 import hook!! --
import System.Environment ( getArgs, getProgName )
import System.Exit ( die )

tmp :: [IO ()]
tmp =
  [ Day01.ReportRepair.main
  , Day02.PasswordPhilosophy.main
  , Day03.TobogganTrajectory.main
  , Day04.PassportProcessing.main
  , Day05.BinaryBoarding.main
  , Day06.CustomCustoms.main
  , Day07.HandyHaversacks.main
  -- !!aoc2020 main hook!! --
  ]

main :: IO ()
main = getArgs >>= parse >>= (tmp !!) . subtract 1

parse :: [String] -> IO Int
parse [day] = return $ (read @Int) day
parse _ = do
    progName <- getProgName
    die $ "Usage: " ++ progName ++ " <day>"