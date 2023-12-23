{-# LANGUAGE TypeApplications #-}
module Main where
import qualified Day01.Trebuchet
import qualified Day02.CubeConundrum
import qualified Day03.GearRatios
import qualified Day04.Scratchcards
import qualified Day05.IfYouGiveASeedAFertilizer
import qualified Day06.WaitForIt
import qualified Day07.CamelCards
import qualified Day08.HauntedWasteland
-- !!import hook!! --
import System.Environment ( getArgs, getProgName )
import System.Exit ( die )

tmp :: [IO ()]
tmp =
  [ 
    Day01.Trebuchet.main
  , Day02.CubeConundrum.main
  , Day03.GearRatios.main
  , Day04.Scratchcards.main
  , Day05.IfYouGiveASeedAFertilizer.main
  , Day06.WaitForIt.main
  , Day07.CamelCards.main
  , Day08.HauntedWasteland.main
  -- !!main hook!! --
  ]

main :: IO ()
main = getArgs >>= parse >>= (tmp !!) . subtract 1

parse :: [String] -> IO Int
parse [day] = return $ (read @Int) day
parse _ = do
    progName <- getProgName
    die $ "Usage: " ++ progName ++ " <day>"