module FloydCycleDetect where

{-|Floyd Cycle Detection

Returns (mu, la) where mu is the start of a cycle, la is the length of the cycle
-}
findCycle :: (Eq n, Integral a) => (n -> n) -> n -> (a, a)
findCycle f x0 =
  let (_, x1) = run (1, 2) (move 1 x0) (move 2 x0) 0
      (mu, x2) = run (1, 1) x0 x1 0
      (la, _) = run (0, 1) x2 (move 1 x2) 1 in (mu,la)
  where
    move v x0 = iterate f x0 !! v
    run (v1, v2) t h mu | t == h = (mu, h)
                        | otherwise = run (v1, v2) (move v1 t) (move v2 h) (mu+1)
