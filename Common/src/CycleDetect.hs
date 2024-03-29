{-# OPTIONS_HADDOCK show-extensions #-}

module CycleDetect (findCycle) where

{-|Floyd Cycle Detection

Returns (mu, la) where:
  mu is the start of a cycle,
  la is the length of the cycle
-}
findCycle :: forall node i. (Eq node, Integral i) => (node -> node) -> node -> (i, i)
findCycle f x0 =
  let (_, x1) = run (1, 2) (move 1 x0) (move 2 x0) (0::i)
      (mu, x2) = run (1, 1) x0 x1 0
      (la, _) = run (0, 1) x2 (move 1 x2) 1
   in (mu, la)
  where
    move v x = iterate f x !! v
    run (v1, v2) tortoise hare mu
      | tortoise == hare = (mu, hare)
      | otherwise = run (v1, v2) (move v1 tortoise) (move v2 hare) (mu+1)
