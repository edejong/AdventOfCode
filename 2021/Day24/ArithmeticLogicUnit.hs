module Day24.ArithmeticLogicUnit where
import Test.QuickCheck
import Data.List (zip4, foldl')

main :: IO ()
main = do
    xs <- readFile "2021/data/day24-test.txt"
    print xs

data Digit = Digit1 | Digit2 | Digit3 | Digit4 | Digit5 | Digit6 | Digit7 | Digit8 | Digit9 deriving (Bounded, Enum, Show)

instance Arbitrary Digit where
    arbitrary = arbitraryBoundedEnum

runProgram :: Digit -> Digit -> Digit -> Digit -> Digit -> Digit -> Digit -> Digit -> Digit -> Digit -> Digit -> Digit -> Digit -> Digit -> Int
runProgram i1 i2 i3 i4 i5 i6 i7 i8 i9 i10 i11 i12 i13 i14 =
    let inputs = map ((+1) . fromEnum) [i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14]
        is = [1,   1, 1,   26,  1,  1,  1, 26,  1, 26,  26,  26, 26, 26]
        js = [11, 11, 15, -11, 15, 15, 14, -7, 12, -6, -10, -15, -9,  0]
        ks = [6,  12,  8,   7,  7, 12,  2, 15,  4,  5,  12,  11, 13,  7]
        (x, y, z, w) = foldl' f (0,0,0,0) (zip4 is js ks inputs) in z
    where f (x, y, z, w) (i, j, k, inp) = loopBody x y z w i j k inp

loopBody :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> (Int, Int, Int, Int)
loopBody x0 y0 z0 w0 i j k inp = do
    let w1 = inp
        x1 = x0 * 0
        x2 = x1 + z0
        x3 = x2 `mod` 26
        z1 = z0 `div` i
        x4 = x3 + j
        x5 = if x4 == w1 then 1 else 0
        x6 = if x5 == 0 then 1 else 0
        y1 = y0 * 0
        y2 = y1 + 25
        y3 = y2 * x6
        y4 = y3 + 1
        z2 = z1 * y4
        y5 = y4 * 0
        y6 = y5 + w1
        y7 = y6 + k
        y8 = y7 * x6
        z3 = z2 + y8
    (x6, y8, z3, w1)

runProgram2 :: Digit -> Digit -> Digit -> Digit -> Digit -> Digit -> Digit -> Digit -> Digit -> Digit -> Digit -> Digit -> Digit -> Digit -> Int
runProgram2 i1 i2 i3 i4 i5 i6 i7 i8 i9 i10 i11 i12 i13 i14 =
    let inputs = map ((+1) . fromEnum) [i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14]
        is = [1,   1, 1,   26,  1,  1,  1, 26,  1, 26,  26,  26, 26, 26]
        js = [11, 11, 15, -11, 15, 15, 14, -7, 12, -6, -10, -15, -9,  0]
        ks = [6,  12,  8,   7,  7, 12,  2, 15,  4,  5,  12,  11, 13,  7]
        z = foldl' f 0 (zip4 is js ks inputs) in z
    where f z (i, j, k, inp) = loopBody2 z i j k inp

loopBody2 :: Int -> Int -> Int -> Int -> Int -> Int
loopBody2 z i j k inp =
    if ((z `mod` 26) + j) /= inp then
        ((z `div` i) * 26) + (inp + k)
    else
        z `div` i

runProgram3 j1 j2 j3 j4 j5 j6 j7 j8 j9 j10 j11 j12 j13 j14 =
    let [i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14] = map ((+1) . fromEnum) [j1, j2, j3, j4, j5, j6, j7, j8, j9, j10, j11, j12, j13, j14]
        z1 = i1 + 6                     -- z1 = [7..15]
        z2 = (z1 * 26) + i2 + 12        -- z2 = [195..411]
        z3 = (z2 * 26) + i3 + 8         -- z3 = [5079..10703]
        z4 = if (i3 - 3) /= i4 then ((z3 `div` 26) * 26) + i4 + 7 else z3 `div` 26 -- z4 = [195..411]
        z5 = (z4 * 26) + i5 + 7         -- z5 = [5078..10702]
        z6 = (z5 * 26) + i6 + 12        -- z6 = [132041..278273]
        z7 = (z6 * 26) + i7 + 2         -- z7 = [3433069..7235109]
        z8 = if (i7 - 5) /= i8 then ((z7 `div` 26) * 26) + i8 + 15 else z7 `div` 26 -- z8 = [132041..278273]
        z9 = (z8 * 26) + i9 + 4         -- z9 = [3433071..7235111]
        z10 = if (i9 - 2) /= i10 then ((z9 `div` 26) * 26) + i10 + 5 else z9 `div` 26 -- z10 = [132041..278273]
        z11 = if ((z10 `mod` 26) - 10) /= i11 then ((z10 `div` 26) * 26) + i11 + 12 else z10 `div` 26 -- z11 = [5078..10702]
        z12 = if ((z11 `mod` 26) - 15) /= i12 then ((z11 `div` 26) * 26) + i12 + 11 else z11 `div` 26 -- z12 = [195..411]
        z13 = if ((z12 `mod` 26) - 9) /= i13 then ((z12 `div` 26) * 26) + i13 + 13 else z12 `div` 26 -- z13 = [7..15]
        z14 = if (z13 `mod` 26) /= i14 then ((z13 `div` 26) * 26) + i14 + 7 else z13 `div` 26 -- z14 = 0
        -- z14 = 0
    in z14


findIt = take 1 [(i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14) |
            i1 <- ds, i2 <- ds, i3 <- ds, i4 <- ds, i5 <- ds, i6 <- ds,
            i7 <- ds, i8 <- ds, i9 <- ds, i10 <- ds, i11 <- ds, i12 <- ds, i13 <- ds, i14 <- ds,
            0 == runProgram2 i1 i2 i3 i4 i5 i6 i7 i8 i9 i10 i11 i12 i13 i14]
  where ds = reverse [Digit1 .. Digit9]


prop_program :: Digit -> Digit -> Digit -> Digit -> Digit -> Digit -> Digit -> Digit -> Digit -> Digit -> Digit -> Digit -> Digit -> Digit  -> Bool
prop_program i1 i2 i3 i4 i5 i6 i7 i8 i9 i10 i11 i12 i13 i14 =
    runProgram i1 i2 i3 i4 i5 i6 i7 i8 i9 i10 i11 i12 i13 i14 == 
    runProgram3 i1 i2 i3 i4 i5 i6 i7 i8 i9 i10 i11 i12 i13 i14