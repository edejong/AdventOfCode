module Array where

import           Control.Lens
import           Data.List    (groupBy)
import           GHC.Arr

showArr2D :: (Ix i, Field1 i i a a, Eq a) => Array i Char -> String
showArr2D = unlines . map (map snd) . groupBy (\ a b -> a^._1._1 == b^._1._1) . assocs
