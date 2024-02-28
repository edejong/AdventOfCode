{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
import           Data.List          (findIndices, sort)
import           Data.List.Split    (splitOn)
import           Text.Parsec        (between, char, digit, many1, parse, sepBy,
                                     (<|>))

main :: IO ()
main = do
    xs <- map (map parsePacket . words) . splitOn "\n\n" <$> readFile "2022/Day13/day13.txt"
    let part1 = sum . map fst . filter ((==LT) . snd) . zip [1..] . map (\[lhs, rhs] -> compare lhs rhs) $ xs
    let dividerPackets = [parsePacket "[[2]]", parsePacket "[[6]]"]
    let part2 = product . map (+1) . findIndices (`elem` dividerPackets) . sort $ concat xs ++ dividerPackets
    print (part1 :: Int, part2)

data PacketData = Int Int | List [PacketData] deriving Eq

parsePacket :: String -> PacketData
parsePacket str = either (error . show) id (parse packet "" str)
  where
    packet = number <|> list
    number = Int . (read @Int) <$> many1 digit
    list = List <$> between (char '[') (char ']') (sepBy packet (char ','))

instance Ord PacketData where
    compare (List xs) (List ys) = compare xs ys
    compare (Int x) (Int y)     = compare x y
    compare (List xs) (Int y)   = compare (List xs) (List [Int y])
    compare (Int x) (List ys)   = compare (List [Int x]) (List ys)
