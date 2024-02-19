module ShortestPath (shortestPaths) where
import           Control.Monad
import           Control.Monad.ST
import           Data.Array
import           Data.Array.ST
import           Data.Maybe

type Arr s = STArray s (Int, Int) (Maybe Int)

shortestPaths :: Int -> (Int -> Int -> Maybe Int) -> Array (Int, Int) (Maybe Int)
shortestPaths numVerts weightFunc = runSTArray $ do
    let initialValues = [weightFunc i j | i <- [1..numVerts], j <- [1..numVerts]]
        arrayBounds = ((1, 1), (numVerts, numVerts))
    dist <- newListArray arrayBounds initialValues :: ST s (Arr s)
    forM_ [1..numVerts] $ \k ->
        forM_ [1..numVerts] $ \i ->
            forM_ [1..numVerts] $ \j -> do
                a <- readArray dist (i, j)
                b <- readArray dist (i, k)
                c <- readArray dist (k, j)
                when (isJust b && isJust c && (isNothing a || fromJust a > fromJust b + fromJust c)) $ do
                    writeArray dist (i, j) ((+) <$> b <*> c)
    return dist
