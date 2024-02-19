{-# OPTIONS_HADDOCK show-extensions #-}

module Geometry (rot90, rot180, rot270, m22_to_m33, translate) where
import           Linear

{- 90 degree CCW rotation matrix -}
rot90 :: Num a => M22 a
rot90 = V2 (V2 0 (-1)) (V2 1 0)

{- 180 degree rotation matrix -}
rot180 :: Num a => M22 a
rot180 = V2 (V2 (-1) 0) (V2 0 (-1))

{- 270 degree CCW rotation matrix -}
rot270 :: Num a => M22 a
rot270 = V2 (V2 0 1) (V2 (-1) 0)

m22_to_m33 :: Num a => M22 a -> M33 a
m22_to_m33 (V2 (V2 a b) (V2 c d)) = V3 (V3 a b 0)
                                       (V3 c d 0)
                                       (V3 0 0 1)

translate :: Num a => V2 a -> M33 a
translate (V2 x y) = V3 (V3 1 0 x)
                        (V3 0 1 y)
                        (V3 0 0 1)
