{-# LANGUAGE TypeApplications #-}
module Main where
import qualified Day01.CalorieCounting
import qualified Day02.RockPaperScissors
import qualified Day03.RucksackReorganization
import qualified Day04.CampCleanup
import qualified Day05.SupplyStacks
import qualified Day06.TuningTrouble
import qualified Day07.NoSpaceLeftOnDevice
import qualified Day08.TreetopTreeHouse
import qualified Day09.RopeBridge
import qualified Day10.CathodeRayTube
import qualified Day11.MonkeyintheMiddle
import qualified Day12.HillClimbingAlgorithm
import qualified Day13.DistressSignal
import qualified Day14.RegolithReservoir
import qualified Day15.BeaconExclusionZone
import qualified Day16.ProboscideaVolcanium
import qualified Day17.PyroclasticFlow
-- !!import hook!! --
import System.Environment ( getArgs, getProgName )
import System.Exit ( die )

tmp :: [IO ()]
tmp =
  [ Day01.CalorieCounting.main
  , Day02.RockPaperScissors.main
  , Day03.RucksackReorganization.main
  , Day04.CampCleanup.main
  , Day05.SupplyStacks.main
  , Day06.TuningTrouble.main
  , Day07.NoSpaceLeftOnDevice.main
  , Day08.TreetopTreeHouse.main
  , Day09.RopeBridge.main
  , Day10.CathodeRayTube.main
  , Day11.MonkeyintheMiddle.main
  , Day12.HillClimbingAlgorithm.main
  , Day13.DistressSignal.main
  , Day14.RegolithReservoir.main
  , Day15.BeaconExclusionZone.main
  , Day16.ProboscideaVolcanium.main
  , Day17.PyroclasticFlow.main
  -- !!main hook!! --
  ]

main :: IO ()
main = getArgs >>= parse >>= (tmp !!) . subtract 1

parse :: [String] -> IO Int
parse [day] = return $ (read @Int) day
parse _ = do
    progName <- getProgName
    die $ "Usage: " ++ progName ++ " <day>"