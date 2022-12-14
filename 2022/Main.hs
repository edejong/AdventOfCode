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
  -- !!main hook!! --
  ]

main :: IO ()
main = getArgs >>= parse >>= (tmp !!) . subtract 1

parse :: [String] -> IO Int
parse [day] = return $ (read @Int) day
parse _ = do
    progName <- getProgName
    die $ "Usage: " ++ progName ++ " <day>"