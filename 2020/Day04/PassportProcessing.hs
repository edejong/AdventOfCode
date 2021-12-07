{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
module Day04.PassportProcessing where
-- import Control.Applicative
import Data.List
import Data.List.Split (splitOn, splitOneOf)
import Text.Read
import Data.Char

main :: IO ()
main = do
    passports <- map (splitOneOf " \n") . splitOn "\n\n" <$> readFile "2020/data/day04.txt"
    let passports1 = filter allRequiredFieldsPresent passports
    let passports2 = filter (all isValid) passports1
    print (length passports1, length passports2)

allRequiredFieldsPresent passport = case (fields \\ ["cid"]) \\ map (head . splitOn ":") passport of
                            [] -> True
                            _ -> False
  where fields = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid", "cid"]

-- >>> isValid "ecl:gry"
-- >>> isValid "pid:860033327"
-- >>> isValid "eyr:2020"
-- >>> isValid "hcl:#fffffd"
-- >>> isValid "byr:1937"
-- >>> isValid "iyr:2017"
-- >>> isValid "cid:147"
-- >>> isValid "hgt:183cm"
-- True
-- True
-- True
-- True
-- True
-- True
-- True
-- False


isValid :: [Char] -> Bool
isValid xs = let val = drop 4 xs in case take 3 xs of
    "byr" -> intBetween 1920 2002 val
    "iyr" -> intBetween 2010 2020 val
    "eyr" -> intBetween 2020 2030 val
    "hgt" -> let h = takeWhile isDigit val in case dropWhile isDigit val of
        "cm" -> intBetween 150 193 h
        "in" -> intBetween 59 76 h
        _ -> False
    "hcl" -> head val == '#' && all isHexDigit (tail val)
    "ecl" -> val `elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
    "pid" -> length val == 9 && all isDigit val
    "cid" -> True
    _ -> False

intBetween :: Int -> Int -> String -> Bool
intBetween a b = maybe False (\x -> x >= a && x <= b) . (readMaybe @Int)

-- pid (Passport ID) - a nine-digit number, including leading zeroes.
-- cid (Country ID) - ignored, missing or not.

-- >>> ['a', 'b', 'd'] \\ ['a', 'b', 'a', 'c']
-- "d"
