import           Data.Bits                     (Bits (shiftR), (.&.))
import           Data.Char                     (digitToInt, intToDigit)
import           Data.Either                   (fromRight)
import           Data.List                     (unfoldr)
import           Numeric                       (readInt, showIntAtBase)
import           Text.Parsec.String            (Parser)
import           Text.ParserCombinators.Parsec

data PacketType = SumType
                | ProductType
                | MinimumType
                | MaximumType
                | LiteralType
                | GreaterThanType
                | LessThanType
                | EqualToType deriving (Enum, Show)

data Packet = Packet Header Body deriving (Show)

data Header = Header { ver :: Int
                     , typ :: PacketType } deriving Show

data Body = Literal Integer
          | SubPackets [Packet] deriving Show

main :: IO ()
main = do
    xs <- readFile "2021/Day16/day16.txt"
    case parse packet "" (hexStringToBin xs) of
        Left err -> print err
        Right p -> do
            print $ sumVersions p
            print $ eval p

---- Helper functions ----
hexDigitToBin :: Char -> String
hexDigitToBin = zipWith (\s c' -> if shiftR c' s .&. 1 == 1 then '1' else '0') [3,2..0] . repeat . digitToInt

hexStringToBin :: String -> String
hexStringToBin = concatMap hexDigitToBin

binToInt :: (Num a) => String -> a
binToInt = fst . head . readInt 2 (const True) digitToInt

---- Packets ----
sumVersions :: Packet -> Int
sumVersions (Packet (Header ver _) body) = ver + case body of
    (SubPackets ps) -> sum . map sumVersions $ ps
    _               -> 0

eval :: Packet -> Integer
eval (Packet (Header ver op) body) = case op of
    SumType         -> sum xs
    ProductType     -> product xs
    MinimumType     -> minimum xs
    MaximumType     -> maximum xs
    LiteralType     -> v
    GreaterThanType -> if a > b then 1 else 0
    LessThanType    -> if a < b then 1 else 0
    EqualToType     -> if a == b then 1 else 0
  where
    xs = map eval subPackets
    (a:b:_) = xs
    (Literal v) = body
    (SubPackets subPackets) = body

---- Parsers ----
packet :: Parser Packet
packet = do
    ver <- int 3
    typ <- toEnum <$> int 3
    body <- bodyForType typ
    return $ Packet (Header ver typ) body

int :: (Num a) => Int -> Parser a
int n = binToInt <$> count n (oneOf "01")

bodyForType :: PacketType -> Parser Body
bodyForType LiteralType = literal
bodyForType _           = operator

literal :: Parser Body
literal = Literal . binToInt <$> literalBin

literalBin :: Parser String
literalBin = (concat <$> many (char '1' *> nibble)) <> (char '0' *> nibble)

nibble :: Parser String
nibble = count 4 (oneOf "01")

operator :: Parser Body
operator = do
    lengthType <- oneOf "01"
    operator' lengthType

operator' :: Char -> Parser Body
operator' '0' = do
    length <- int 15
    subPackets <- subPacketsOfTotalLength length
    return $ SubPackets subPackets
operator' '1' = do
    length <- int 11
    subPackets <- count length packet
    return $ SubPackets subPackets
operator' _ = error "THIS SHOULD NEVER HAPPEN"

subPacketsOfTotalLength :: Int -> Parser [Packet]
subPacketsOfTotalLength 0 = return []
subPacketsOfTotalLength n = do
    s <- count n (oneOf "01")
    return $ case parse (many packet) "" s of
               Left e  -> error (show e)
               Right p -> p
