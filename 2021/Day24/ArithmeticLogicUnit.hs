
main :: IO ()
main = do
    xs <- map words . lines <$> readFile "2021/Day24/day24.txt"
    let (is,js,ks) = (readConsts 4 xs, readConsts 5 xs, readConsts 15 xs)
    print $ head (optimised False is js ks)
    print $ head (optimised True is js ks)

readConsts :: Int -> [[String]] -> [Int]
readConsts l xs =
    let subLen = length xs `div` 14
        f []  = []
        f xs' = (read . last . head $ xs') : f (drop subLen xs')
     in f (drop l xs)

-- run :: [[String]] -> V4 Int -> [Int] -> Int
-- run instructions st inputs =
--      let (st', _) = foldl' (flip instrP) (st, inputs) instructions
--       in st' ^._z

-- instrP :: [String] -> (V4 Int, [Int]) -> (V4 Int, [Int])
-- instrP [] st = st
-- instrP ["inp", "w"] st = instrP ["inp", "w", "1"] st
-- instrP [x, reg1, reg2] (st,inputs) =
--     case x of
--         "inp" -> (st & _a .~ head inputs, tail inputs)
--         "add" -> (st & _a %~ (+ b), inputs)
--         "mul" -> (st & _a %~ (* b), inputs)
--         "mod" -> (st & _a %~ (`mod` b), inputs)
--         "div" -> (st & _a %~ (`div` b), inputs)
--         "eql" -> (st & _a %~ (fromEnum . (== b)), inputs)
--         _     -> error $ "unhandled instruction: " ++ show x
--   where
--     _a = r reg1
--     _b = r reg2
--     b = if isDigit . last $ reg2 then read @Int reg2 else st ^. _b
--     r "x" = _x
--     r "y" = _y
--     r "z" = _z
--     r "w" = _w
--     r  _  = undefined
-- instrP _ _ = undefined

optimised :: Bool -> [Int] -> [Int] ->[Int] ->[[Int]]
optimised asc = f 0
  where
    ds = if asc then [1..9] else [9,8..1]
    f z (i:is') (j:js') (k:ks') =
        [ inp:rest | inp <- ds
        , (i /= 26) || (z `mod` 26 == (inp - j))
        , let z' = if i == 26 then z `div` i else z `div` i * 26 + inp + k
        , rest <- f z' is' js' ks'
        ]
    f 0 [] [] [] = [[]]
    f _ _ _ _ = []
