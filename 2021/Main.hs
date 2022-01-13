{-# LANGUAGE TypeApplications #-}
module Main where
import qualified Day01.SonarSweep
import qualified Day02.Dive
import qualified Day03.BinaryDiagnostic
import qualified Day04.Bingo
import qualified Day05.HydrothermalVenture
import qualified Day06.Lanternfish
import qualified Day07.TheTreacherOfWhales
import qualified Day08.SevenSegmentSearch
import qualified Day09.SmokeBasin
import qualified Day10.SyntaxScoring
import qualified Day11.DumboOctopus
import qualified Day12.PassagePathing
import qualified Day13.TransparentOrigami
import qualified Day14.ExtendedPolymerization
import qualified Day15.Chiton
import qualified Day16.PacketDecoder
import qualified Day17.TrickShot
import qualified Day18.Snailfish
import qualified Day19.BeaconScanner
import qualified Day20.TrenchMap
-- !!aoc2021 import hook!! --
import System.Environment ( getArgs, getProgName )
import System.Exit ( die )

tmp :: [IO ()]
tmp =
  [
    Day01.SonarSweep.main
  , Day02.Dive.main
  , Day03.BinaryDiagnostic.main
  , Day04.Bingo.main
  , Day05.HydrothermalVenture.main
  , Day06.Lanternfish.main
  , Day07.TheTreacherOfWhales.main
  , Day08.SevenSegmentSearch.main
  , Day09.SmokeBasin.main
  , Day10.SyntaxScoring.main
  , Day11.DumboOctopus.main
  , Day12.PassagePathing.main
  , Day13.TransparentOrigami.main
  , Day14.ExtendedPolymerization.main
  , Day15.Chiton.main
  , Day16.PacketDecoder.main
  , Day17.TrickShot.main
  , Day18.Snailfish.main
  , Day19.BeaconScanner.main
  , Day20.TrenchMap.main
  -- !!aoc2021 main hook!! --
  ]

main :: IO ()
main = getArgs >>= parse >>= (tmp !!) . subtract 1

parse :: [String] -> IO Int
parse [day] = return $ (read @Int) day
parse _ = do
    progName <- getProgName
    die $ "Usage: " ++ progName ++ " <day>"