{-# LANGUAGE BlockArguments #-}

module FloydWarshall where

import Control.Monad.ST ( ST )
import Control.Monad (forM_, when)
import Data.Array (Array)
import Data.Array.ST.Safe
    ( STArray, newListArray, readArray, writeArray, runSTArray )
import Data.Maybe (isNothing, isJust, fromJust, fromMaybe)

weight :: Int -> Int -> Maybe Int
weight 1 3 = Just (-2)
weight 2 1 = Just 4
weight 2 3 = Just 3
weight 3 4 = Just 2
weight 4 2 = Just (-1)
weight a b | a == b = Just 0
weight _ _ = Nothing

type Arr s = STArray s (Int, Int) (Maybe Int)

shortestPaths :: Int -> (Int -> Int -> Maybe Int) -> Array (Int, Int) (Maybe Int)
shortestPaths numVerts weightFunc = runSTArray do
    let initialValues = [weightFunc i j | i <- [1..numVerts], j <- [1..numVerts]]
        arrayBounds = ((1, 1), (numVerts, numVerts))
    dist <- newListArray arrayBounds initialValues :: ST s (Arr s)
    forM_ [1..numVerts] $ \k ->
        forM_ [1..numVerts] $ \i ->
            forM_ [1..numVerts] $ \j -> do
                a <- readArray dist (i, j)
                b <- readArray dist (i, k)
                c <- readArray dist (k, j)
                when (isJust b && isJust c && (isNothing a || fromJust a > fromJust b + fromJust c)) do
                    writeArray dist (i, j) ((+) <$> b <*> c)
    return dist
