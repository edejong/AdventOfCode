
main :: IO ()
main = do
    xs <- lines <$> readFile "2022/Day25/day25.txt"
    print $ unSnafu . sum $ map snafuP xs

snafuP :: String -> Integer
snafuP = sum . zipWith (*) (iterate (*5) 1) . (map fromChar . reverse)

unSnafu :: Integer -> String
unSnafu n | n <= 2 = [toChar n]
          | otherwise =
            let (n1,n2) = ((n+2) `div` 5, (n+2) `rem` 5) in unSnafu n1 ++ unSnafu (n2-2)

fromChar :: Char -> Integer
fromChar = \case '2' ->  2; '1' ->  1; '0' ->  0; '-' -> -1; '=' -> -2; _ -> undefined

toChar :: Integer -> Char
toChar  = \case 2 -> '2'; 1 -> '1'; 0 -> '0'; (-1) -> '-'; (-2) -> '='; _ -> undefined
