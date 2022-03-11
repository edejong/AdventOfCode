{-# LANGUAGE TypeApplications #-}
module Main where
import qualified Day01.NotQuiteLisp
import qualified Day02.IWasToldThereWouldBeNoMath
import qualified Day03.PerfectlySphericalHousesinaVacuum
-- !!import hook!! --
import System.Environment ( getArgs, getProgName )
import System.Exit ( die )

tmp :: [IO ()]
tmp =
  [ Day01.NotQuiteLisp.main
  , Day02.IWasToldThereWouldBeNoMath.main
  , Day03.PerfectlySphericalHousesinaVacuum.main
  -- !!main hook!! --
  ]

main :: IO ()
main = getArgs >>= parse >>= (tmp !!) . subtract 1

parse :: [String] -> IO Int
parse [day] = return $ (read @Int) day
parse _ = do
    progName <- getProgName
    die $ "Usage: " ++ progName ++ " <day>"