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

{- Floyd Cycle Detection
Returns (mu, la) where mu is the start of a cycle, la is the length of the cycle
-}
floyd :: Eq a => (a -> a) -> a -> (Integer, Integer)
floyd f x0 =
  let (_, x1) = run (1, 2) (move 1 x0) (move 2 x0) 0
      (mu, x2) = run (1, 1) x0 x1 0
      (la, _) = run (0, 1) x2 (move 1 x2) 1 in (mu,la)
  where
    move v x0 = iterate f x0 !! v
    run (v1, v2) t h mu | t == h = (mu, h)
                        | otherwise = run (v1, v2) (move v1 t) (move v2 h) (mu+1)
