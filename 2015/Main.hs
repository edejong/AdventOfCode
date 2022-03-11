{-# LANGUAGE TypeApplications #-}
module Main where
import qualified Day01.NotQuiteLisp
import qualified Day02.IWasToldThereWouldBeNoMath
-- !!import hook!! --
import System.Environment ( getArgs, getProgName )
import System.Exit ( die )

tmp :: [IO ()]
tmp =
  [ Day01.NotQuiteLisp.main
  , Day02.IWasToldThereWouldBeNoMath.main
  -- !!main hook!! --
  ]

main :: IO ()
main = getArgs >>= parse >>= (tmp !!) . subtract 1

parse :: [String] -> IO Int
parse [day] = return $ (read @Int) day
parse _ = do
    progName <- getProgName
    die $ "Usage: " ++ progName ++ " <day>"